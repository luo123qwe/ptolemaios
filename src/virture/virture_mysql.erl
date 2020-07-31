%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 数据库用的5.7
%%% 数据缓存和落地
%%% 原则上只有单个进程能够操作数据
%%% 通常一组表由一类进程操作, 并且在进程初始化的时候就加锁
%%% 所以这里的方法不提供锁检测, 需要上层保证加锁
%%% 例如操作某个玩家的数据, 应该为那个玩家加上锁
%%% 而不是仅对某个表加锁
%%% todo fix record
%%% todo dets
%%% @end
%%%-------------------------------------------------------------------
-module(virture_mysql).

-include("virture.hrl").

%% 基本操作
-export([init/2, lookup/2, insert/1, delete/2, fold_cache/3, make_ets_name/1]).

%% 手动回滚, 刷到数据库
-export([flush/0, hold/0, rollback/1, clean/0, clean/1, all_table/0]).

%% 算是private
-export([build_table/0, init_ets/0, check_flush/0, flush_dets/0]).

-export([base_test/0]).

-type json_def() :: term().
-type field_type() :: int32|int64|uint32|uint64|float|string|to_string|binary|to_binary|json_def().
-export_type([field_type/0]).

%% @doc 自动创建数据表
-spec build_table() -> [{Table :: atom(), Error :: term()|exists|ok}].
build_table() ->
    lists:map(fun(Virture) ->
        Fields = [[atom_to_list(Field), $ , convert_type(Type), " NOT NULL,"] || #vmysql_field{name = Field, type = Type} <- Virture#vmysql.all_fields],
        PrivateKey = ["primary key(", string:join([atom_to_list(Field) || Field <- Virture#vmysql.private_key], ","), $)],
        Sql = ["create table ", atom_to_list(Virture#vmysql.table), "(", Fields, PrivateKey, ")ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"],
%%        io:format("~s~n", [Sql]),
        case mysql_poolboy:query(?VMYSQL_POOL, Sql) of
            {error, {1050, _, _}} ->% 表已经存在
                {Virture#vmysql.table, exists};
            {error, Error} ->
                {Virture#vmysql.table, Error};
            _ ->
                {Virture#vmysql.table, ok}
        end
              end, virture_config:all(mysql)).

convert_type(?VIRTURE_INT32) ->
    "int";
convert_type(?VIRTURE_UINT32) ->
    "int unsigned";
convert_type(?VIRTURE_INT64) ->
    "bigint";
convert_type(?VIRTURE_UINT64) ->
    "bigint unsigned";
convert_type(?VIRTURE_FLOAT) ->
    "float";
convert_type(?VIRTURE_STRING) ->
    "text";
convert_type(?VIRTURE_TO_STRING) ->
    "text";
convert_type(?VIRTURE_BINARY) ->
    "blob";
convert_type(?VIRTURE_TO_BINARY) ->
    "blob";
convert_type(?VIRTURE_JSON_LIST) ->
    "json";
convert_type(?VIRTURE_JSON_OBJ(_)) ->
    "json";
convert_type(?VIRTURE_JSON_OBJ_LIST(_)) ->
    "json".

%% @doc 初始化ets
-spec init_ets() -> term().
init_ets() ->
    lists:foreach(fun(Virture) ->
        EtsName = make_ets_name(Virture),
        EtsName = ets:new(EtsName, [{keypos, Virture#vmysql.private_key_pos} | lists:keydelete(keypos, 1, Virture#vmysql.ets_opt)])
                  end, virture_config:all(mysql)).

%% @doc 在当前进程初始化一个表, SelectKey为[]时搜索全部数据
-spec init(atom(), undefined|list()|term()) -> term().
init(Table, undefined) ->
    %% 初始化缓存
    Virture1 = init_virture(Table),
    %% 初始化进程字典
    init_pd(Virture1),
    %% 定时检查落地
    case Virture1#vmysql.sync_time of
        After when is_integer(After) ->
            erlang:send_after(After * 1000, self(), vmysql_sync);
        _ ->
            skip
    end;
init(Table, SelectKey) when is_list(SelectKey) ->
    %% 初始化缓存
    Virture1 = init_virture(Table),
    %% 初始化数据
    Virture2 = init_data(SelectKey, Virture1),
    %% 初始化进程字典
    init_pd(Virture2),
    %% 定时检查落地
    case Virture2#vmysql.sync_time of
        After when is_integer(After) ->
            erlang:send_after(After * 1000, self(), vmysql_sync);
        _ ->
            skip
    end;
init(Table, SelectKey) ->
    init(Table, [SelectKey]).

%% 查询一条数据
-spec lookup(atom(), list()) -> Record :: tuple()|undefined.
lookup(Table, Key) when is_list(Key) ->
    %% 先从变更的缓存中拿
    case lookup_from_change(Key, Table) of
        delete ->
            undefined;
        undefined ->
            %% 从缓存中拿
            lookup_from_cache(Key, Table);
        Record ->
            Record
    end;
lookup(Table, Key) ->
    lookup(Table, [Key]).

%% 先从变更的缓存中拿
lookup_from_change(Key, Table) ->
    TableChangeList = get(?PD_VMYSQL_CHANGE),
    case lists:keyfind(Table, #vmysql_change.table, TableChangeList) of
        false -> undefined;
        #vmysql_change{private_key_pos = PKPos, state_pos = StatePos, data = Data} ->
            case get_from_data(Key, PKPos, Data) of
                undefined ->
                    undefined;
                Record ->
                    %% 判断是否已删除
                    case element(StatePos, Record) of
                        ?VIRTURE_STATE_DELETE ->
                            delete;
                        _ ->
                            Record
                    end
            end
    end.

%% 从缓存中拿
lookup_from_cache(Key, Table) ->
    Virture = get({?PD_VMYSQL_CACHE, Table}),
    get_from_data(Key, Virture#vmysql.private_key_pos, Virture#vmysql.data).


%% 插入一条数据
-spec insert(tuple()) -> term().
insert(Record) ->
    Table = element(1, Record),
    #vmysql{
        state_pos = StatePos, private_key_pos = PrivateKeyPos,
        private_key = PrivateKey, data = VData
    } = Virture = get({?PD_VMYSQL_CACHE, Table}),
    Record1 = setelement(StatePos, Record, ?VIRTURE_STATE_REPLACE),
    %% 新数据自动构造key
    case element(PrivateKeyPos, Record1) of
        undefined ->
            Key = make_private_key(PrivateKey, Record1),
            Record2 = setelement(PrivateKeyPos, Record1, Key);
        Key ->
            Record2 = Record1
    end,
    ChangeList = get(?PD_VMYSQL_CHANGE),
    %% 表是否有change缓存
    case lists:keytake(Table, #vmysql_change.table, ChangeList) of
        {value, #vmysql_change{data = Data} = Change, ChangeList1} ->
            Data1 = set_to_data(Key, PrivateKeyPos, Record2, Data),
            Change1 = Change#vmysql_change{data = Data1},
            put(?PD_VMYSQL_CHANGE, [Change1 | ChangeList1]),
            %% 检查是否需要删除原本的缓存
            case data_size(Data) =/= data_size(Data1) of
                true ->
                    VData1 = delete_from_data(Key, PrivateKeyPos, VData),
                    Virture1 = Virture#vmysql{data = VData1},
                    put({?PD_VMYSQL_CACHE, Table}, Virture1),
                    check_sync(data_size(Data1), Virture1);
                false ->
                    check_sync(data_size(Data1), Virture)
            end;
        _ ->
            Change = make_change(Key, Record2, Virture),
            put(?PD_VMYSQL_CHANGE, [Change | get(?PD_VMYSQL_CHANGE)]),
            VData1 = delete_from_data(Key, PrivateKeyPos, VData),
            Virture1 = Virture#vmysql{data = VData1},
            put({?PD_VMYSQL_CACHE, Table}, Virture1),
            check_sync(1, Virture1)
    end.

%% @doc 删除一条数据
-spec delete(atom(), list()|term()) -> term().
delete(Table, Key) when is_list(Key) ->
    ChangeList = get(?PD_VMYSQL_CHANGE),
    #vmysql{
        private_key_pos = PrivateKeyPos,
        state_pos = StatePos, data = VData
    } = Virture = get({?PD_VMYSQL_CACHE, Table}),
    %% 构造record, 不需要全部的record字段, 关键数据在就可以
    Record = erlang:make_tuple(max(StatePos, PrivateKeyPos), undefined, [{1, Table}, {PrivateKeyPos, Key}, {StatePos, ?VIRTURE_STATE_DELETE}]),
    case lists:keytake(Table, #vmysql_change.table, ChangeList) of
        {value, #vmysql_change{data = Data} = Change, ChangeList1} ->
            Data1 = set_to_data(Key, PrivateKeyPos, Record, Data),
            Change1 = Change#vmysql_change{data = Data1},
            %% 检查是否需要删除原本的缓存
            case data_size(Data) =/= data_size(Data1) of
                true ->
                    VData1 = delete_from_data(Key, PrivateKeyPos, VData),
                    Virture1 = Virture#vmysql{data = VData1};
                false ->
                    Virture1 = Virture
            end;
        _ ->
            Change1 = make_change(Key, Record, Virture),
            ChangeList1 = ChangeList,
            VData1 = delete_from_data(Key, PrivateKeyPos, VData),
            Virture1 = Virture#vmysql{data = VData1}
    end,
    put(?PD_VMYSQL_CHANGE, [Change1 | ChangeList1]),
    put({?PD_VMYSQL_CACHE, Table}, Virture1),
    check_sync(data_size(Change1#vmysql_change.data), Virture1);
delete(Table, Key) ->
    delete(Table, [Key]).

%% 构造change结构并且插入第一条数据
make_change(Key, Record, Virture) ->
    #vmysql_change{
        table = Virture#vmysql.table,
        private_key_pos = Virture#vmysql.private_key_pos,
        state_pos = Virture#vmysql.state_pos,
        data = set_to_data(Key, Virture#vmysql.private_key_pos, Record, get_empty_data(Virture#vmysql.data))
    }.

%% 检查是否触发同步
check_sync(CurrentSize, #vmysql{table = Table, sync_size = SyncSize}) ->
    case CurrentSize >= SyncSize of
        true ->
            case get(?PD_VMYSQL_FLUSH) of
                FlushList when is_list(FlushList) ->
                    case lists:member(Table, FlushList) of
                        true -> ok;
                        _ -> put(?PD_VMYSQL_FLUSH, [Table | FlushList])
                    end;
                _ ->
                    put(?PD_VMYSQL_FLUSH, [Table])
            end;
        _ -> skip
    end.

%% @doc 遍历当前数据
-spec fold_cache(fun((Key :: term(), Record :: term(), Acc :: term())-> Acc1 :: term()), term(), atom()) -> term().
fold_cache(F, Acc, Table) ->
    #vmysql{private_key_pos = PKPos, data = Data} = get({?PD_VMYSQL_CACHE, Table}),
    ChangeTableList = get(?PD_VMYSQL_CHANGE),
    case lists:keyfind(Table, #vmysql_change.table, ChangeTableList) of
        #vmysql_change{data = ChangeData} ->
            Acc1 = fold_data(F, Acc, PKPos, ChangeData),
            fold_data(F, Acc1, PKPos, Data);
        _ ->
            fold_data(F, Acc, PKPos, Data)
    end.

%% 检查是否存在需要flush的表
-spec check_flush() -> term().
check_flush() ->
    case get(?PD_VMYSQL_FLUSH) of
        List when is_list(List), List =/= [] ->
            flush(List);
        _ ->
            skip
    end.

%% @doc flush所有改变的数据到数据库
-spec flush() -> ok.
flush() ->
    flush_change(get(?PD_VMYSQL_CHANGE), []).

flush_change([], FailList) ->
    put(?PD_VMYSQL_CHANGE, FailList),
    ok;
flush_change([H | T], FailList) ->
    case catch do_flush(H) of
        ok ->
            flush_change(T, FailList);
        _Error ->
            flush_change(T, [H | FailList])
    end.

%% flush到数据库, 这个接口不开放了
%% 保证数据一致性, 用户应该把全部flush或者不flush
flush(TableList) ->
    flush(TableList, get(?PD_VMYSQL_CHANGE)).

flush([], ChangeList) ->
    put(?PD_VMYSQL_CHANGE, ChangeList),
    ok;
flush([Table | T], ChangeList) ->
    case lists:keytake(Table, #vmysql_change.table, ChangeList) of
        {value, Change, ChangeList1} ->
            case catch do_flush(Change) of
                ok ->
                    flush(T, ChangeList1);
                _Error ->
                    flush(T, ChangeList)
            end;
        _ ->
            flush(T, ChangeList)
    end.

do_flush(#vmysql_change{table = Table, private_key_pos = PKPos, data = ChangeData}) ->
    %% 拿到cache
    #vmysql{
        ets = EtsName,
        data = Data, state_pos = StatePos, private_key = PrivateKey,
        replace_sql = ReplaceSql, delete_sql = DeleteSql, private_where_sql = WhereSql,
        all_fields = AllFields
    } = Virture = get({?PD_VMYSQL_CACHE, Table}),
    %% 拼sql, 统计改变
    {Data1, ReplaceIOList, DeleteIOList, ReplaceList, DeleteList} =
        fold_data(fun(Key, Value, {D, RIO, DIO, RL, DL}) ->
            case element(StatePos, Value) of
                ?VIRTURE_STATE_REPLACE ->
                    Value1 = setelement(StatePos, Value, ?VIRTURE_STATE_NOT_CHANGE),
                    D1 = set_to_data(Key, PKPos, Value1, D),
                    [[_, First] | Left] = [[$,, encode(Type, element(Pos, Value))] || #vmysql_field{type = Type, pos = Pos} <- AllFields],
                    RIO1 = [$,, $(, First, Left, $) | RIO],
                    {D1, RIO1, DIO, [Value1 | RL], DL};
                ?VIRTURE_STATE_DELETE ->
                    WhereSql1 = join_where(WhereSql, PrivateKey, Key),
                    DIO1 = [DeleteSql, WhereSql1, $; | DIO],
                    {D, RIO, DIO1, RL, [Key | DL]};
                Unknow ->
                    %% ???????
                    throw({flush, Unknow})
            end
                  end, {Data, [], [], [], []}, PKPos, ChangeData),
    case ReplaceIOList of
        [_ | ReplaceIOList1] -> ok;
        _ -> ReplaceIOList1 = []
    end,
    %% 执行sql
    MysqlResult =
        if
            ReplaceIOList1 =/= [] ->
                mysql_poolboy:query(?VMYSQL_POOL, [ReplaceSql, ReplaceIOList1, $;, DeleteIOList]);
            DeleteIOList =/= [] ->
                mysql_poolboy:query(?VMYSQL_POOL, DeleteIOList);
            true ->
                ok
        end,
    case MysqlResult of
        {error, Reason} ->
            %% todo 服务器关闭时保存到dets
            throw({mysql, Reason});
        _ ->
            %% 保存数据
            %% ets
            lists:foreach(fun(Record) ->
                ets:insert(EtsName, Record)
                          end, ReplaceList),
            lists:foreach(fun(Key) ->
                ets:delete(EtsName, Key)
                          end, DeleteList),
            %% 缓存
            Virture1 = Virture#vmysql{data = Data1},
            put({?PD_VMYSQL_CACHE, Table}, Virture1),
            ok
    end.


%% @doc mysql失败的数据保存到dets
flush_dets() ->
    flush_dets(get(?PD_VMYSQL_CHANGE)).

flush_dets([]) ->
    ok;
flush_dets([#vmysql_change{table = Table, data = Data} | T]) ->
    #vmysql{private_key_pos = PKPos} = virture_config:get(mysql, Table),
    case dets:open_file(Table, [{file, ?VMYSQL_DETS_PATH ++ "/" ++ atom_to_list(Table)}, {keypos, PKPos}]) of
        {ok, Table} ->
            fold_data(fun(_K, V, _) ->
                dets:insert(Table, V)
                      end, [], PKPos, Data);
        Error ->
            Error
    end,
    flush_dets(T).

%% @doc 获取当前状态, 用于回滚
-spec hold() -> #vmysql_hold{}.
hold() ->
    ChangeTableList = get(?PD_VMYSQL_CHANGE),
    Cache =
        lists:foldl(fun(#vmysql_change{table = Table}, Acc) ->
            [get({?PD_VMYSQL_CACHE, Table}) | Acc]
                    end, [], ChangeTableList),
    #vmysql_hold{cache = Cache, change = ChangeTableList, flush = get(?PD_VMYSQL_FLUSH)}.

%% @doc hold()的状态回滚
-spec rollback(#vmysql_hold{}) -> term().
rollback(#vmysql_hold{cache = Cache, change = ChangeTableList, flush = Flush}) ->
    put(?PD_VMYSQL_CHANGE, ChangeTableList),
    lists:foreach(fun(#vmysql{table = Table} = Virture) ->
        put({?PD_VMYSQL_CACHE, Table}, Virture)
                  end, Cache),
    put(?PD_VMYSQL_FLUSH, Flush).

%% @equiv clean(all_table())
clean() ->
    clean(all_table()).

%% @doc 清理缓存
-spec clean(list()) -> term().
clean(TableList) ->
    clean(TableList, get(?PD_VMYSQL_CHANGE), get(?PD_VMYSQL_FLUSH)).

clean([], ChangeList, Flush) ->
    put(?PD_VMYSQL_CHANGE, ChangeList),
    put(?PD_VMYSQL_FLUSH, Flush),
    ok;
clean([Table | T], ChangeList, Flush) ->
    erase({?PD_VMYSQL_CACHE, Table}),
    clean(T, lists:keydelete(Table, #vmysql_change.table, ChangeList), lists:delete(Table, Flush)).

%% @doc 获取全部table
-spec all_table() -> list().
all_table() ->
    lists:foldl(fun
                    ({?PD_VMYSQL_CACHE, Table}, Acc) ->
                        [Table | Acc];
                    (_, Acc) ->
                        Acc
                end, [], get_keys()).

%% 从data中取出record
get_from_data(Key, PrivateKeyPos, ?VIRTURE_LIST(_Size, List)) when is_list(List) ->
    case lists:keyfind(Key, PrivateKeyPos, List) of
        false -> undefined;
        Record -> Record
    end;
get_from_data(Key, _PrivateKeyPos, Dict) ->
    case dict:find(Key, Dict) of
        error -> undefined;
        {ok, Record} -> Record
    end.

%% 保存record到data
set_to_data(Key, PrivateKeyPos, Value, ?VIRTURE_LIST(Size, List)) ->
    case lists:keytake(Key, PrivateKeyPos, List) of
        false ->
            ?VIRTURE_LIST(Size + 1, [Value | List]);
        {value, _, List1} ->
            ?VIRTURE_LIST(Size, [Value | List1])
    end;
set_to_data(Key, _PrivateKeyPos, Value, Dict) ->
    dict:store(Key, Value, Dict).

%% 从data中删除record
delete_from_data(Key, PrivateKeyPos, ?VIRTURE_LIST(Size, List) = VirtureList) when is_list(List) ->
    case lists:keytake(Key, PrivateKeyPos, List) of
        false ->
            VirtureList;
        {value, _, List1} ->
            ?VIRTURE_LIST(Size - 1, List1)
    end;
delete_from_data(Key, _PrivateKeyPos, Dict) ->
    dict:erase(Key, Dict).

%% 遍历data
fold_data(F, Acc, PrivateKeyPos, ?VIRTURE_LIST(_Size, List)) when is_list(List), is_function(F, 3) ->
    fold_data_list(F, Acc, PrivateKeyPos, List);
fold_data(F, Acc, _PrivateKeyPos, Dict) ->
    dict:fold(F, Acc, Dict).

fold_data_list(_F, Acc, _PrivateKeyPos, []) ->
    Acc;
fold_data_list(F, Acc, PrivateKeyPos, [H | T]) ->
    fold_data_list(F, F(element(PrivateKeyPos, H), H, Acc), PrivateKeyPos, T).

data_size(?VIRTURE_LIST(Size, _List)) ->
    Size;
data_size(Dict) ->
    dict:size(Dict).

%% 获取一个空的data结构
get_empty_data(?VIRTURE_LIST(_Size, List)) when is_list(List) ->
    ?VIRTURE_LIST(0, []);
get_empty_data(_) ->
    dict:new().

%% 初始化
init_virture(Table) ->
    #vmysql{all_fields = AllField, private_key = PrivateKey, select_key = Selectkey} = Virture = virture_config:get(mysql, Table),
    %% 初始化优化sql
    Virture#vmysql{
        ets = make_ets_name(Virture),
        private_key = [lists:keyfind(Field, #vmysql_field.name, AllField) || Field <- PrivateKey],
        select_key = [lists:keyfind(Field, #vmysql_field.name, AllField) || Field <- Selectkey],
        private_where_sql = make_private_where_sql(Virture),
        select_where_sql = make_select_where_sql(Virture),
        select_sql = make_select_sql(Virture),
        replace_sql = make_replace_sql(Virture),
        delete_sql = make_delete_sql(Virture)
    }.

%% ets名字
make_ets_name(#vmysql{table = Table}) ->
    make_ets_name(Table);
make_ets_name(Table) ->
    list_to_atom("ets_" ++ atom_to_list(Table)).

make_private_where_sql(Virture) ->
    case Virture#vmysql.private_key of
        [H] ->
            [" WHERE " ++ atom_to_list(H) ++ "="];
        [H | T] ->
            HSql = " WHERE " ++ atom_to_list(H) ++ "=",
            TSql = [" AND " ++ atom_to_list(Name) ++ "=" || Name <- T],
            [HSql | TSql]
    end.

make_select_where_sql(Virture) ->
    case Virture#vmysql.select_key of
        [] ->
            [];
        [H] ->
            [" WHERE " ++ atom_to_list(H) ++ "="];
        [H | T] ->
            HSql = " WHERE " ++ atom_to_list(H) ++ "=",
            TSql = [" AND " ++ atom_to_list(Name) ++ "=" || Name <- T],
            [HSql | TSql]
    end.

make_select_sql(Virture) ->
    AllField = string:join([atom_to_list(VK#vmysql_field.name) || VK <- Virture#vmysql.all_fields], ","),
    "SELECT " ++ AllField ++ " FROM " ++ atom_to_list(Virture#vmysql.table).

make_replace_sql(Virture) ->
    AllField = string:join([atom_to_list(VK#vmysql_field.name) || VK <- Virture#vmysql.all_fields], ","),
    ["REPLACE INTO " ++ atom_to_list(Virture#vmysql.table) ++ " (" ++ AllField ++ ")VALUES"].

make_delete_sql(Virture) ->
    "DELETE FROM " ++ atom_to_list(Virture#vmysql.table).

%% 拼where的sql
join_where([], [], []) ->
    [];
join_where([SqlH | SqlT], [#vmysql_field{type = Type} | FieldT], [Value | ValueT]) ->
    [SqlH, encode(Type, Value) | join_where(SqlT, FieldT, ValueT)].

%% 初始化数据
init_data(Key, Virture) ->
    #vmysql{
        ets = EtsName,
        private_key_pos = PrivateKeyPos, state_pos = StatePos,
        private_key = PrivateKey, select_key = SelectKey,
        select_where_sql = WhereSql, select_sql = SelectSql,
        record_size = RecordSize,
        data = VData, init_fun = InitFun
    } = Virture,
    %% 先从ets拿
    EtsData =
        case Key of
            [] ->
                %% 搜索全部
                ets:tab2list(EtsName);
            _ ->
                Spec = erlang:make_tuple(RecordSize, '_'),
                KeySpec = make_ets_key_spec(PrivateKey, SelectKey, Key),
                Spec1 = setelement(PrivateKeyPos, Spec, KeySpec),
                ets:select(EtsName, [{Spec1, [], ['$_']}])
        end,
    case EtsData of
        [] ->
            %% 从数据库拿
            WhereSql1 =
                case Key of
                    [] -> [];
                    _ -> join_where(WhereSql, SelectKey, Key)
                end,
            {ok, _ColumnNames, Rows} = mysql_poolboy:query(?VMYSQL_POOL, [SelectSql, WhereSql1]),
            %% 构造record数据
            Data =
                lists:foldl(fun(Row, Acc) ->
                    Record = init_record(InitFun, Row, Virture),
                    %% 构造key
                    K = make_private_key(PrivateKey, Record),
                    Record1 = setelement(PrivateKeyPos, Record, K),
                    %% 设置状态
                    Record2 = erlang:setelement(StatePos, Record1, ?VIRTURE_STATE_NOT_CHANGE),
                    set_to_data(K, PrivateKeyPos, Record2, Acc)
                            end, VData, Rows),
            %% 全部初始化成功再放到ets
            fold_data(fun(_K, Value, _Acc) ->
                ets:insert(EtsName, Value)
                      end, [], PrivateKeyPos, Data),
            Virture#vmysql{data = Data};
        _EtsData ->
            %% 构造本地缓存
            Data =
                lists:foldl(fun(Record, Acc) ->
                    set_to_data(element(PrivateKeyPos, Record), PrivateKeyPos, Record, Acc)
                            end, VData, EtsData),
            Virture#vmysql{data = Data}
    end.

%% 初始化进程字典
init_pd(Virture) ->
    put({?PD_VMYSQL_CACHE, Virture#vmysql.table}, Virture),
    case get(?PD_VMYSQL_CHANGE) of
        undefined -> put(?PD_VMYSQL_CHANGE, []);
        _ -> skip
    end,
    case get(?PD_VMYSQL_FLUSH) of
        undefined -> put(?PD_VMYSQL_FLUSH, []);
        _ -> skip
    end.


make_private_key(PrivateKey, Record) ->
    [element(Pos, Record) || #vmysql_field{pos = Pos} <- PrivateKey].


make_ets_key_spec([], _SelectKey, _Value) ->
    [];
make_ets_key_spec([#vmysql_field{pos = Pos} | PKT], SelectKey, ValueList) ->
    case make_ets_key_spec_find_value(Pos, SelectKey, ValueList) of
        false ->
            ['_' | make_ets_key_spec(PKT, SelectKey, ValueList)];
        Value ->
            [Value | make_ets_key_spec(PKT, SelectKey, ValueList)]
    end.

make_ets_key_spec_find_value(_Pos, [], []) ->
    false;
make_ets_key_spec_find_value(Pos, [#vmysql_field{pos = Pos} | _FT], [Value | _VT]) ->
    Value;
make_ets_key_spec_find_value(Pos, [_ | FT], [_ | VT]) ->
    make_ets_key_spec_find_value(Pos, FT, VT).

encode(to_string, Value) ->
    io_lib:format("'~w'", [Value]);
%% 用的数据库是5.7, 加上_binary才不会报Warning 1300: Invalid utf8mb4 character string
encode(to_binary, Value) ->
    ["_binary ", mysql_encode:encode(erlang:term_to_binary(Value))];
encode(binary, Value) ->
    ["_binary ", mysql_encode:encode(Value)];
encode(?VIRTURE_JSON_LIST, List) ->
    encode_json_list(List);
encode(?VIRTURE_JSON_OBJ(NameList), Value) ->
    encode_json_obj(NameList, Value);
encode(?VIRTURE_JSON_OBJ_LIST(NameList), Value) ->
    encode_json_obj_list(NameList, Value);
encode(_, Value) ->
    mysql_encode:encode(Value).

encode_json_list(List) ->
    ["JSON_ARRAY(", do_encode_json_list(List), $)].

do_encode_json_list([]) ->
    [];
do_encode_json_list([H]) ->
    mysql_encode:encode(H);
do_encode_json_list([H | T]) ->
    [mysql_encode:encode(H), $, | do_encode_json_list(T)].

encode_json_obj(NameList, Value) ->
    ["JSON_OBJECT(", do_encode_json_obj(NameList, tuple_to_list(Value)), $)].

do_encode_json_obj([], []) ->
    [];
do_encode_json_obj([N], [V]) ->
    [do_encode_json_single_obj(N, V)];
do_encode_json_obj([N | NT], [V | VT]) ->
    [do_encode_json_single_obj(N, V), $, | do_encode_json_obj(NT, VT)].

encode_json_obj_list(NameList, List) ->
    ["JSON_ARRAY(", do_encode_json_obj_list(NameList, List), $)].

do_encode_json_obj_list(_NameList, []) ->
    [];
do_encode_json_obj_list(NameList, [V]) ->
    encode_json_obj(NameList, V);
do_encode_json_obj_list(NameList, [V | VT]) ->
    [encode_json_obj(NameList, V), $, | do_encode_json_obj_list(NameList, VT)].

do_encode_json_single_obj(?VIRTURE_JSON_LIST(Name), V) ->
    [$', Name, $', $,, encode_json_list(V)];
do_encode_json_single_obj(?VIRTURE_JSON_OBJ(Name, NameList), V) ->
    [$', Name, $', $,, encode_json_obj(NameList, V)];
do_encode_json_single_obj(?VIRTURE_JSON_OBJ_LIST(Name, NameList), V) ->
    [$', Name, $', $,, encode_json_obj_list(NameList, V)];
do_encode_json_single_obj(Name, V) ->
    [$', Name, $', $,, mysql_encode:encode(V)].


decode(to_string, Value) ->
    util:eval(Value);
decode(to_binary, Value) ->
    erlang:binary_to_term(Value);
decode(?VIRTURE_JSON_LIST, Value) ->
    jsx:decode(Value);
decode(?VIRTURE_JSON_OBJ(NameList), Value) ->
    decode_json_obj(NameList, jsx:decode(Value));
decode(?VIRTURE_JSON_OBJ_LIST(NameList), Value) ->
    decode_json_obj_list(NameList, jsx:decode(Value));
decode(_, Value) ->
    Value.

decode_json_obj(NameList, Map) ->
    list_to_tuple([decode_json_single_obj(Name, Map) || Name <- NameList]).

decode_json_obj_list(NameList, List) ->
    [decode_json_obj(NameList, Obj) || Obj <- List].

decode_json_single_obj(?VIRTURE_JSON_LIST(Name), Map) ->
    jsx:decode(maps:get(Name, Map));
decode_json_single_obj(?VIRTURE_JSON_OBJ(Name, NameList), Map) ->
    decode_json_obj(NameList, maps:get(Name, Map));
decode_json_single_obj(?VIRTURE_JSON_OBJ_LIST(Name, NameList), Map) ->
    decode_json_obj_list(NameList, maps:get(Name, Map));
decode_json_single_obj(Name, Map) ->
    maps:get(Name, Map).

%% 初始化record
init_record(Init, Row, Virture) ->
    BaseRecord = erlang:make_tuple(Virture#vmysql.record_size, undefined, [{1, Virture#vmysql.table}]),
    init_record(Init, Row, Virture#vmysql.all_fields, BaseRecord).

init_record(InitFun, [], [], Record) ->
    case InitFun of
        {M, F} -> M:F(Record);
        _ -> Record
    end;
init_record(InitFun, [Value | ValueT], [#vmysql_field{pos = Pos, type = Type} | FieldT], Record) ->
    init_record(InitFun, ValueT, FieldT, setelement(Pos, Record, decode(Type, Value))).

%% 不想写_SUIT, 凑合用着先吧
base_test() ->
    %% 创建test数据表
    mysql_poolboy:query(?VMYSQL_POOL, "drop table if exists vmysql_test_player;
    create table vmysql_test_player(player_id int,str varchar(100),to_str varchar(100),to_bin blob,to_json json,primary key(player_id));
    drop table if exists vmysql_test_goods;
    create table vmysql_test_goods(player_id int,goods_id int,str varchar(100),to_str varchar(100),to_bin blob,primary key(player_id,goods_id));"),
    %% 初始化
    virture_mysql:init(vmysql_test_player, [1]),
    virture_mysql:init(vmysql_test_goods, [1]),
    %% 插入数据
    undefined = virture_mysql:lookup(vmysql_test_player, 1),
    undefined = virture_mysql:lookup(vmysql_test_goods, [1, 1]),
    virture_mysql:insert(#vmysql_test_player{player_id = 1, str = <<"1">>, to_str = <<"1">>, to_bin = <<"1">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
    virture_mysql:insert(#vmysql_test_player{player_id = 2, str = <<"2">>, to_str = <<"2">>, to_bin = <<"2">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 1, goods_id = 1, str = <<"1">>, to_str = <<"1">>, to_bin = <<"1">>}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 1, goods_id = 2, str = <<"2">>, to_str = <<"2">>, to_bin = <<"2">>}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 2, goods_id = 1, str = <<"1">>, to_str = <<"1">>, to_bin = <<"1">>}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 2, goods_id = 2, str = <<"2">>, to_str = <<"2">>, to_bin = <<"2">>}),
    0 = ets:info(virture_mysql:make_ets_name(vmysql_test_player), size),
    0 = ets:info(virture_mysql:make_ets_name(vmysql_test_goods), size),
    virture_mysql:check_flush(),
    2 = ets:info(virture_mysql:make_ets_name(vmysql_test_player), size),
    4 = ets:info(virture_mysql:make_ets_name(vmysql_test_goods), size),
%%    io:format("~p~n", [get()]),
    %% 插入+更新+删除
    virture_mysql:insert(#vmysql_test_player{player_id = 1, str = <<"11">>, to_str = <<"11">>, to_bin = <<"11">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
    virture_mysql:delete(vmysql_test_player, 2),
    virture_mysql:insert(#vmysql_test_goods{player_id = 1, goods_id = 1, str = <<"11">>, to_str = <<"11">>, to_bin = <<"11">>}),
    virture_mysql:delete(vmysql_test_goods, [1, 2]),
    virture_mysql:insert(#vmysql_test_player{player_id = 3, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 3, goods_id = 1, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 3, goods_id = 2, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>}),
    #vmysql_test_player{str = <<"11">>} = virture_mysql:lookup(vmysql_test_player, 1),
    undefined = virture_mysql:lookup(vmysql_test_player, 2),
    #vmysql_test_player{str = <<"3">>} = virture_mysql:lookup(vmysql_test_player, 3),
    #vmysql_test_goods{str = <<"11">>} = virture_mysql:lookup(vmysql_test_goods, [1, 1]),
    undefined = virture_mysql:lookup(vmysql_test_goods, [1, 2]),
    #vmysql_test_goods{str = <<"3">>} = virture_mysql:lookup(vmysql_test_goods, [3, 1]),
%%    io:format("~p~n", [get()]),
    virture_mysql:check_flush(),
    2 = ets:info(virture_mysql:make_ets_name(vmysql_test_player), size),
    5 = ets:info(virture_mysql:make_ets_name(vmysql_test_goods), size),
    #vmysql_test_player{str = <<"11">>} = virture_mysql:lookup(vmysql_test_player, 1),
    undefined = virture_mysql:lookup(vmysql_test_player, 2),
    #vmysql_test_player{str = <<"3">>} = virture_mysql:lookup(vmysql_test_player, 3),
    #vmysql_test_goods{str = <<"11">>} = virture_mysql:lookup(vmysql_test_goods, [1, 1]),
    undefined = virture_mysql:lookup(vmysql_test_goods, [1, 2]),
    #vmysql_test_goods{str = <<"3">>} = virture_mysql:lookup(vmysql_test_goods, [3, 1]),
    %% hold
    Hold = virture_mysql:hold(),
    virture_mysql:insert(#vmysql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 1, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 2, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
    #vmysql_test_player{} = virture_mysql:lookup(vmysql_test_player, 4),
    #vmysql_test_goods{} = virture_mysql:lookup(vmysql_test_goods, [4, 1]),
    #vmysql_test_goods{} = virture_mysql:lookup(vmysql_test_goods, [4, 2]),
    virture_mysql:rollback(Hold),
    2 = ets:info(virture_mysql:make_ets_name(vmysql_test_player), size),
    5 = ets:info(virture_mysql:make_ets_name(vmysql_test_goods), size),
    undefined = virture_mysql:lookup(vmysql_test_player, 4),
    undefined = virture_mysql:lookup(vmysql_test_goods, [4, 1]),
    undefined = virture_mysql:lookup(vmysql_test_goods, [4, 2]),
    %% all_table
    [] = virture_mysql:all_table()--[vmysql_test_player, vmysql_test_goods],
    virture_mysql:clean(virture_mysql:all_table()),
    [] = virture_mysql:all_table(),
    %% flush
    virture_mysql:init(vmysql_test_player, [1]),
    virture_mysql:init(vmysql_test_goods, [1]),
    virture_mysql:insert(#vmysql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 1, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 2, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
    2 = ets:info(virture_mysql:make_ets_name(vmysql_test_player), size),
    5 = ets:info(virture_mysql:make_ets_name(vmysql_test_goods), size),
    flush([vmysql_test_goods]),
    2 = ets:info(virture_mysql:make_ets_name(vmysql_test_player), size),
    7 = ets:info(virture_mysql:make_ets_name(vmysql_test_goods), size),
    flush([vmysql_test_goods]),
    2 = ets:info(virture_mysql:make_ets_name(vmysql_test_player), size),
    7 = ets:info(virture_mysql:make_ets_name(vmysql_test_goods), size),
    ok.
