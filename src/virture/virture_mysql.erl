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
%%% @end
%%%-------------------------------------------------------------------
-module(virture_mysql).

-include("virture.hrl").
-include("util.hrl").

%% 基本操作
-export([init/2, init/3, lookup/2, dirty_lookup/2, insert/1, delete/2, fold_cache/3, make_ets_name/1]).

%% 手动回滚, 刷到数据库
-export([flush/0, hold/0, rollback/1, clean/0, clean/1, all_table/0]).

%% 算是private
-export([build_table/0, build_table/1, init_system/0, check_flush/0, flush_dets/0, fix_dets/3, check_dets/0, hotfix/2]).

-export([base_test/0]).

-type json_def() :: term().
-type field_type() :: int32|int64|uint32|uint64|float|string|to_string|binary|to_binary|json_def().
-export_type([field_type/0]).

%% @doc 自动创建数据表
-spec build_table() -> [{Table :: atom(), Error :: term()|exists|ok}].
build_table() ->
    build_table(virture_config:all(mysql)).

build_table(VirtureList) ->
    lists:map(fun(Virture) ->
        Fields = [[atom_to_list(Field), $ , convert_type(Type), " NOT NULL,"]
            || #vmysql_field{name = Field, type = Type} <- Virture#vmysql.all_fields],
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
              end, VirtureList).

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

%% @doc 初始化
-spec init_system() -> term().
init_system() ->
    case filelib:is_dir(?VMYSQL_DETS_PATH) of
        true -> check_dets();
        _ -> file:make_dir(?VMYSQL_DETS_PATH)
    end,
    {ok, ?VMYSQL_DETS} = dets:open_file(?VMYSQL_DETS, [{file, ?VMYSQL_DETS_PATH ++ "/" ++ atom_to_list(?VMYSQL_DETS)}, {keypos, #vmysql.table}]),
    lists:foreach(fun(Virture) ->
        EtsName = make_ets_name(Virture),
        EtsName = ets:new(EtsName, [{keypos, Virture#vmysql.private_key_pos} | lists:keydelete(keypos, 1, Virture#vmysql.ets_opt)]),
        dets:insert(?VMYSQL_DETS, Virture)
                  end, virture_config:all(mysql)).

%% @equiv init(Table, Key, undefined)
init(Table, SelectKey) ->
    init(Table, SelectKey, undefined).

%% @doc 在当前进程初始化一个表
%% SelectKey为[]时搜索全部数据
%% WhereSql为额外的where语句, 因为预设不满足大部分情况
-spec init(atom(), undefined|list(), undefined|iolist()) -> term().
init(Virture = #vmysql{}, SelectKey, WhereSql) ->
    %% 初始化缓存
    Virture1 = init_virture(Virture),
    %% 初始化数据
    Virture2 =
        case SelectKey of
            undefined -> Virture1;
            _ -> init_data(SelectKey, WhereSql, Virture1)
        end,
    %% 初始化进程字典
    init_pd(Virture2),
    %% 定时检查落地
    case Virture2#vmysql.sync_time of
        After when is_integer(After) ->
            erlang:send_after(After * 1000, self(), vmysql_sync);
        _ ->
            skip
    end;
init(Table, SelectKey, WhereSql) ->
    Virture = virture_config:get(mysql, Table),
    init(Virture, SelectKey, WhereSql).


%% 查询一条数据
-spec lookup(atom(), list()) -> Record :: tuple()|undefined.
lookup(Table, Key) when is_list(Key) ->
    case get(?PD_VMYSQL) of
        #{Table := #vmysql{state_pos = StatePos, data = Data, change = ChangeData}} ->
            %% 先从变更的缓存中拿
            case lookup_from_change(Key, StatePos, ChangeData) of
                delete ->
                    undefined;
                undefined ->
                    %% 从缓存中拿
                    maps:get(Key, Data, undefined);
                Record ->
                    Record
            end;
        _ ->
            undefined
    end.

%% 先从变更的缓存中拿
lookup_from_change(Key, StatePos, Data) ->
    case maps:get(Key, Data, undefined) of
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
    end.


%% @doc 脏读, 先读ets, 再读数据库
%% 不会读缓存中的数据
%% 数据不会保存到进程字典缓存中
-spec dirty_lookup(atom(), list()) -> undefined|tuple().
dirty_lookup(Table, Key) ->
    case ets:lookup(make_ets_name(Table), Key) of
        [Record] ->
            Record;
        _ ->
            case get(?PD_VMYSQL) of
                #{Table := Virture} ->
                    dirty_lookup_db(Virture, Key);
                _ ->
                    Virture = init_virture(virture_config:get(mysql, Table)),
                    dirty_lookup_db(Virture, Key)
            end
    end.

dirty_lookup_db(#vmysql{
    table = Table,
    private_key = PrivateKey, private_key_pos = PrivateKeyPos, state_pos = StatePos,
    select_where_sql = WhereSql, select_sql = SelectSql, select_key = SelectKey,
    init_fun = InitFun
} = Virture, PKey) ->
    Key = private_key_to_select_key(SelectKey, PrivateKey, PKey),
    EtsName = make_ets_name(Table),
    %% 从数据库拿
    WhereSql1 =
        case Key of
            [] -> [];
            _ -> join_where(WhereSql, SelectKey, Key)
        end,
    {ok, _ColumnNames, Rows} = mysql_poolboy:query(?VMYSQL_POOL, [SelectSql, WhereSql1]),
    %% 构造record数据
    {FindRecord, Data} =
        lists:foldl(fun(Row, {FR, D}) ->
            Record = init_record(InitFun, Row, Virture),
            %% 构造key
            K = make_private_key(PrivateKey, Record),
            Record1 = setelement(PrivateKeyPos, Record, K),
            %% 设置状态
            Record2 = erlang:setelement(StatePos, Record1, ?VIRTURE_STATE_NOT_CHANGE),
            case K == PKey of
                true -> {Record2, [Record2 | D]};
                _ -> {FR, [Record2 | D]}
            end
                    end, {undefined, []}, Rows),
    %% 全部初始化成功再放到ets
    lists:foreach(fun(Record) ->
        %% 读进程可以很多个, 但是写进程只有一个
        %% 保证初始化不会覆盖即可
        ets:insert_new(EtsName, Record)
                  end, Data),
    FindRecord.

%% 根据主键构造搜索键
private_key_to_select_key([], _PrivateList, _ValueList) ->
    [];
private_key_to_select_key([#vmysql_field{pos = Pos} | ST], PrivateList, ValueList) ->
    [private_key_to_select_key_1(Pos, PrivateList, ValueList) | private_key_to_select_key(ST, PrivateList, ValueList)].

private_key_to_select_key_1(Pos, [#vmysql_field{pos = Pos} | _PT], [V | _VT]) ->
    V;
private_key_to_select_key_1(Pos, [_ | PT], [_ | VT]) ->
    private_key_to_select_key_1(Pos, PT, VT).

%% 插入一条数据
-spec insert(tuple()) -> term().
insert(Record) ->
    Table = element(1, Record),
    #{Table := #vmysql{
        state_pos = StatePos, private_key_pos = PrivateKeyPos,
        private_key = PrivateKey, data = Data, change = ChangeData
    } = Virture} = VMysql = get(?PD_VMYSQL),
    Record1 = setelement(StatePos, Record, ?VIRTURE_STATE_REPLACE),
    %% 新数据自动构造key
    case element(PrivateKeyPos, Record1) of
        undefined ->
            Key = make_private_key(PrivateKey, Record1),
            Record2 = setelement(PrivateKeyPos, Record1, Key);
        Key ->
            Record2 = Record1
    end,
    %% 表是否有change缓存
    ChangeData1 = ChangeData#{Key => Record2},
    ChangeSize1 = maps:size(ChangeData1),
    %% 检查是否需要删除原本的缓存
    case ChangeSize1 =/= maps:size(ChangeData) of
        true ->
            Data1 = maps:remove(Key, Data),
            Virture1 = Virture#vmysql{data = Data1, change = ChangeData1},
            put(?PD_VMYSQL, VMysql#{Table => Virture1}),
            check_sync(ChangeSize1, Virture1);
        false ->
            Virture1 = Virture#vmysql{change = ChangeData1},
            put(?PD_VMYSQL, VMysql#{Table => Virture1}),
            check_sync(ChangeSize1, Virture)
    end.

%% @doc 删除一条数据
-spec delete(atom(), list()) -> term().
delete(Table, Key) when is_list(Key) ->
    #{Table := #vmysql{
        state_pos = StatePos, private_key_pos = PrivateKeyPos,
        data = Data, change = ChangeData
    } = Virture} = VMysql = get(?PD_VMYSQL),
    %% 构造record, 不需要全部的record字段, 关键数据在就可以
    Record = erlang:make_tuple(max(StatePos, PrivateKeyPos), undefined, [{1, Table}, {PrivateKeyPos, Key}, {StatePos, ?VIRTURE_STATE_DELETE}]),
    ChangeData1 = ChangeData#{Key => Record},
    ChangeSize1 = maps:size(ChangeData1),
    %% 检查是否需要删除原本的缓存
    case ChangeSize1 =/= maps:size(ChangeData) of
        true ->
            Data1 = maps:remove(Key, Data),
            Virture1 = Virture#vmysql{data = Data1, change = ChangeData1},
            put(?PD_VMYSQL, VMysql#{Table => Virture1}),
            check_sync(ChangeSize1, Virture1);
        false ->
            Virture1 = Virture#vmysql{change = ChangeData1},
            put(?PD_VMYSQL, VMysql#{Table => Virture1}),
            check_sync(ChangeSize1, Virture)
    end.

%% 检查是否触发同步
check_sync(CurrentSize, #vmysql{table = Table, sync_size = SyncSize}) ->
    case CurrentSize >= SyncSize of
        true ->
            FlushList = get(?PD_VMYSQL_FLUSH),
            case lists:member(Table, FlushList) of
                true -> ok;
                _ -> put(?PD_VMYSQL_FLUSH, [Table | FlushList])
            end;
        _ -> skip
    end.

%% @doc 遍历当前数据
-spec fold_cache(fun((Key :: term(), Record :: term(), Acc :: term())-> Acc1 :: term()), term(), atom()) -> term().
fold_cache(F, Acc, Table) ->
    #{Table := #vmysql{data = Data, change = ChangeData}} = get(?PD_VMYSQL),
    Acc1 = maps:fold(F, Acc, ChangeData),
    maps:fold(F, Acc1, Data).

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
    case get(?PD_VMYSQL) of
        VMysql when is_map(VMysql) ->
            VMysql1 =
                maps:fold(fun(Table, Virture, Acc) ->
                    case catch do_flush(Virture) of
                        {ok, Virture1} ->
                            Acc#{Table => Virture1};
                        Error ->
                            ?LOG_ERROR("flush error~n~p", [Error]),
                            Acc
                    end
                          end, VMysql, VMysql),
            put(?PD_VMYSQL, VMysql1),
            put(?PD_VMYSQL_FLUSH, []);
        _ ->
            ok
    end.

%% flush到数据库, 这个接口不开放了
%% 保证数据一致性, 用户应该把全部flush或者不flush
flush(TableList) ->
    flush(TableList, get(?PD_VMYSQL_FLUSH), get(?PD_VMYSQL)).

flush([], FlushList, VMysql) ->
    put(?PD_VMYSQL_FLUSH, FlushList),
    put(?PD_VMYSQL, VMysql),
    ok;
flush([Table | T], FlushList, VMysql) ->
    case VMysql of
        #{Table := Virture} ->
            case catch do_flush(Virture) of
                {ok, Virture1} ->
                    VMysql1 = VMysql#{Table => Virture1},
                    flush(T, lists:delete(Table, FlushList), VMysql1);
                Error ->
                    ?LOG_ERROR("flush error~n~p", [Error]),
                    flush(T, FlushList, VMysql)
            end;
        _ ->
            flush(T, FlushList, VMysql)
    end.

do_flush(#vmysql{change = Change} = Virture) when map_size(Change) == 0 ->
    {ok, Virture};
do_flush(Virture) ->
    #vmysql{
        ets = EtsName, state_pos = StatePos, private_key = PrivateKey,
        replace_sql = ReplaceSql, delete_sql = DeleteSql, private_where_sql = WhereSql,
        all_fields = AllFields, data = Data, change = ChangeData
    } = Virture,
    %% 拼sql, 统计改变
    {Data1, ReplaceIOList, DeleteIOList, ReplaceList, DeleteList} =
        maps:fold(fun(Key, Record, {D, RIO, DIO, RL, DL}) ->
            case element(StatePos, Record) of
                ?VIRTURE_STATE_REPLACE ->
                    Value1 = setelement(StatePos, Record, ?VIRTURE_STATE_NOT_CHANGE),
                    D1 = D#{Key => Record},
                    [[_, First] | Left] = [[$,, encode(Type, element(Pos, Record))] || #vmysql_field{type = Type, pos = Pos} <- AllFields],
                    RIO1 = [$,, $(, First, Left, $) | RIO],
                    {D1, RIO1, DIO, [Value1 | RL], DL};
                ?VIRTURE_STATE_DELETE ->
                    WhereSql1 = join_where(WhereSql, PrivateKey, Key),
                    DIO1 = [DeleteSql, WhereSql1, $; | DIO],
                    {D, RIO, DIO1, RL, [Key | DL]};
                Unknow ->
                    %% ???????
                    throw({flush, unknow, Unknow})
            end
                  end, {Data, [], [], [], []}, ChangeData),
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
            {ok, Virture#vmysql{data = Data1, change = #{}}}
    end.


%% @doc mysql失败的数据保存到dets
flush_dets() ->
    case get(?PD_VMYSQL) of
        VMysql when is_map(VMysql) ->
            FailList =
                maps:fold(fun(Table, #vmysql{change = ChangeData}, Acc) ->
                    #vmysql{private_key_pos = PKPos} = virture_config:get(mysql, Table),
                    case dets:open_file(Table, [{file, ?VMYSQL_DETS_PATH ++ "/" ++ atom_to_list(Table)}, {keypos, PKPos}]) of
                        {ok, Table} ->
                            maps:fold(fun(_K, V, _) ->
                                dets:insert(Table, V)
                                      end, [], ChangeData),
                            dets:close(Table),
                            Acc;
                        Error ->
                            [Error | Acc]
                    end
                          end, [], VMysql),
            ?DO_IF_NOT(FailList == [], ?LOG_ERROR("flush dets fail~n~p", [FailList]));
        _ ->
            skip
    end.

%% @doc 检查dets
check_dets() ->
    {ok, ?VMYSQL_DETS} = dets:open_file(?VMYSQL_DETS, [{file, ?VMYSQL_DETS_PATH ++ "/" ++ atom_to_list(?VMYSQL_DETS)}, {keypos, #vmysql.table}]),
    filelib:fold_files(?VMYSQL_DETS_PATH, ".*", false,
        fun(FileName, _Acc) ->
            Table = list_to_atom(filename:basename(FileName)),
            case Table of
                ?VMYSQL_DETS ->
                    skip;
                _ ->
                    [Virture] = dets:lookup(?VMYSQL_DETS, Table),
                    case dets:open_file(Table, [{file, FileName}, {keypos, Virture#vmysql.private_key_pos}]) of
                        {ok, Table} ->
                            case dets:info(Table, size) > 0 of
                                true ->
                                    throw({vmysql, dets, noempty, Table});
                                _ ->
                                    dets:close(Table),
                                    file:delete(FileName)
                            end;
                        Error ->
                            throw({vmysql, dets, error, Error})
                    end
            end
        end, []).

%% @doc 修复dets数据, 同步到数据库
%% 提供两个钩子, 全部修复开始前, 全部修复成功后
%% Fun函数里面必须包含insert或者delete操作, 否则数据会丢失
%% 简单的修复, A数据在dets, 仅使用正常的数据+坏掉数据A修复
%% 如果涉及复杂修复, 请直接使用dets和sql修复, 因为这里无法处理坏掉数据A+坏掉数据B交叉
-spec fix_dets(undefined|function(), fun((Record :: tuple())-> term()), undefined|function()) -> term().
fix_dets(Before, Fun, After) ->
    Self = self(),
    OldTrapExit = erlang:process_flag(trap_exit, true),
    Spawn =
        spawn_link(fun() ->
            {ok, ?VMYSQL_DETS} = dets:open_file(?VMYSQL_DETS, [{file, ?VMYSQL_DETS_PATH ++ "/" ++ atom_to_list(?VMYSQL_DETS)}, {keypos, #vmysql.table}]),
            ?DO_IF(is_function(Before), Before()),
            filelib:fold_files(?VMYSQL_DETS_PATH, ".*", false,
                fun(FileName, _Acc) ->
                    Table = list_to_atom(filename:basename(FileName)),
                    case Table of
                        ?VMYSQL_DETS ->
                            skip;
                        _ ->
                            [Virture] = dets:lookup(?VMYSQL_DETS, Table),
                            case dets:open_file(Table, [{file, FileName}, {keypos, Virture#vmysql.private_key_pos}]) of
                                {ok, Table} ->
                                    ok = dets:sync(Table),
                                    %% 拿到全部数据
                                    AllRecord = dets:select(Table, [{'_', [], ['$_']}]),
                                    %% 初始化, 没有自动同步
                                    Virture1 = init_virture(Virture#vmysql{sync_time = undefined, sync_size = undefined}),
                                    %% 初始化进程字典
                                    init_pd(Virture1),
                                    do_fix_dets(Fun, Table, 0, [], AllRecord),
                                    %% 更新dets配置
                                    dets:insert(?VMYSQL_DETS, virture_config:get(mysql, Table)),
                                    %% 删除dets
                                    dets:close(Table),
                                    file:delete(FileName);
                                Error ->
                                    throw({vmysql, dets, error, Error})
                            end
                    end
                end, []),
            ?DO_IF(is_function(After), After()),
            Self ! fix
                   end),
    Result =
        receive
            {'EXIT', Spawn, Reason} ->
                {error, Reason};
            fix ->
                ok
        end,
    erlang:process_flag(trap_exit, OldTrapExit),
    Result.

do_fix_dets(_Fun, Table, _Len, FixList, []) ->
    do_fix_dets_sync(Table, FixList);
do_fix_dets(Fun, Table, Len, FixList, RemainList) when Len >= ?VMYSQL_FIX_LIMIT ->
    do_fix_dets_sync(Table, FixList),
    do_fix_dets(Fun, Table, 0, [], RemainList);
do_fix_dets(Fun, Table, Len, FixList, [H | T]) ->
    Fun(H),
    do_fix_dets(Fun, Table, Len, [H | FixList], T).

do_fix_dets_sync(_Table, []) ->
    [];
do_fix_dets_sync(Table, FixList) ->
    %% 同步数据库
    flush(),
    %% 检查是否同步成功, 其他数据默认会成功
    case get(?PD_VMYSQL) of
        #{Table := #vmysql{change = #{}}} ->
            %% 删除dets
            lists:foreach(fun(Record) -> dets:delete_object(Table, Record) end, FixList);
        _ ->
            %% 同步失败
            throw({vmysql, sync_dets})
    end.

%% @doc 获取当前状态, 用于回滚
-spec hold() -> {list(), map()}.
hold() ->
    {get(?PD_VMYSQL_FLUSH), get(?PD_VMYSQL)}.

%% @doc hold()的状态回滚
-spec rollback({list(), map()}) -> term().
rollback({FlushList, VMysql}) ->
    put(?PD_VMYSQL_FLUSH, FlushList),
    put(?PD_VMYSQL, VMysql).

%% @equiv clean(all_table())
clean() ->
    erase(?PD_VMYSQL),
    erase(?PD_VMYSQL_FLUSH).

%% @doc 清理缓存
-spec clean(list()) -> term().
clean(TableList) ->
    clean(TableList, get(?PD_VMYSQL), get(?PD_VMYSQL_FLUSH)).

clean([], VMysql, Flush) ->
    put(?PD_VMYSQL, VMysql),
    put(?PD_VMYSQL_FLUSH, Flush),
    ok;
clean([Table | T], VMysql, Flush) ->
    clean(T, maps:remove(Table, VMysql), lists:delete(Table, Flush)).

%% @doc 获取全部table
-spec all_table() -> list().
all_table() ->
    case get(?PD_VMYSQL) of
        VMysql when is_map(VMysql) -> maps:keys(VMysql);
        _ -> []
    end.

%% @doc 热修复某个表
%% 如果定义变了(is_diff_def/2) ,Fun函数里面必须包含insert或者delete操作, 否则数据会丢失
%% 因为ets热修复不会进行重置
%% 所以ets参数不会覆盖, private_key_pos不能改变
%% 简单的修复, A数据在缓存, 仅使用正常的数据+坏掉数据A修复
%% 如果涉及复杂修复, 请直接使用fold_cache和ets和sql修复, 因为这里无法处理坏掉数据A+坏掉数据B交叉
hotfix(Table, Fun) ->
    Hold = hold(),
    #{Table := Virture} = VMsql = get(?PD_VMYSQL),
    case catch do_hotfix(Fun, Virture, VMsql) of
        ok -> ok;
        Error ->
            ?LOG_ERROR("hotfix~n~p", [Error]),
            rollback(Hold),
            error
    end.

do_hotfix(Fun, #vmysql{table = Table, private_key_pos = PKPos, data = Data, change = ChangeData} = OldVirture, VMsql) ->
    case virture_config:get(mysql, Table) of
        #vmysql{} = NewConfig ->
            case PKPos == NewConfig#vmysql.private_key_pos of
                true -> skip;
                _ ->
                    %% 主键位置不能改变因为ets无法中途变更
                    throw({vmysql, Table, private_key_pos, cant, change})
            end,
            NewVirture =
                case is_same_def(OldVirture, NewConfig) of
                    true ->% 数据定义没改变, 杂项更新
                        OldVirture#vmysql{
                            init_fun = NewConfig#vmysql.init_fun,
                            sync_time = NewConfig#vmysql.sync_time,
                            sync_size = NewConfig#vmysql.sync_size
                        };
                    _ ->
                        %% 初始化新的
                        init_virture(NewConfig)
                end,
            VMsql1 = VMsql#{Table => NewVirture},
            put(?PD_VMYSQL, VMsql1),
            %% 执行Fun
            WarpFun = fun(_K, R, _Acc) -> Fun(R) end,
            maps:fold(WarpFun, [], Data),
            maps:fold(WarpFun, [], ChangeData),
            %% 刷到ets
            do_hotfix_flush_ets(Table);
        _ ->% 表被删除了
            VMsql1 = maps:remove(Table, VMsql),
            put(?PD_VMYSQL, VMsql1),
            put(?PD_VMYSQL_FLUSH, lists:delete(Table, get(?PD_VMYSQL_FLUSH)))
    end,
    ok.

do_hotfix_flush_ets(Table) ->
    #{Table := #vmysql{state_pos = StatePos, change = ChangeData}} = get(?PD_VMYSQL),
    Ets = make_ets_name(Table),
    maps:fold(fun(K, R, _Acc) ->
        case element(StatePos, R) of
            ?VIRTURE_STATE_DELETE -> ets:delete(Ets, K);
            ?VIRTURE_STATE_REPLACE -> ets:insert(Ets, R)
        end
              end, [], ChangeData).

%% 定义是否改变
is_same_def(Old, New) ->
    Old#vmysql.state_pos == New#vmysql.state_pos
        andalso Old#vmysql.select_key == New#vmysql.select_key
        andalso Old#vmysql.private_key == New#vmysql.private_key
        andalso Old#vmysql.all_fields == New#vmysql.all_fields
        andalso Old#vmysql.record_size == New#vmysql.record_size.

%% 初始化
init_virture(#vmysql{all_fields = AllField, private_key = PrivateKey, select_key = Selectkey} = Virture) ->
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
init_data(Key, ExtWhereSql, Virture) ->
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
        if
            ExtWhereSql =/= undefined ->
                [];
            Key == [] ->
                %% 搜索全部
                ets:tab2list(EtsName);
            true ->
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
                    [] ->
                        case ExtWhereSql of
                            undefined -> [];
                            _ -> [" WHERE ", ExtWhereSql]
                        end;
                    _ ->
                        case ExtWhereSql of
                            undefined -> join_where(WhereSql, SelectKey, Key);
                            _ -> [join_where(WhereSql, SelectKey, Key), " AND ", ExtWhereSql]
                        end
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
                    Acc#{K => Record2}
                            end, VData, Rows),
            %% 全部初始化成功再放到ets
            maps:fold(fun(_K, Value, _Acc) ->
                %% 读进程可以很多个, 但是写进程只有一个
                %% 保证初始化不会覆盖即可
                ets:insert_new(EtsName, Value)
                      end, [], Data),
            Virture#vmysql{data = Data};
        _EtsData ->
            %% 构造本地缓存
            Data =
                lists:foldl(fun(Record, Acc) ->
                    Acc#{element(PrivateKeyPos, Record) => Record}
                            end, VData, EtsData),
            Virture#vmysql{data = Data}
    end.

%% 初始化进程字典
init_pd(Virture) ->
    case get(?PD_VMYSQL) of
        undefined ->
            put(?PD_VMYSQL, #{Virture#vmysql.table => Virture});
        VMysql ->
            put(?PD_VMYSQL, VMysql#{Virture#vmysql.table => Virture})
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
    %% 删除test数据表
    mysql_poolboy:query(?VMYSQL_POOL, "drop table if exists vmysql_test_player;
    drop table if exists vmysql_test_goods;"),
    %% build
    io:format("~w~n", [virture_mysql:build_table([virture_config:get(mysql, vmysql_test_player), virture_config:get(mysql, vmysql_test_goods)])]),
    %% 初始化
    virture_mysql:init(vmysql_test_player, [1]),
    virture_mysql:init(vmysql_test_goods, [1]),
    %% 插入数据
    undefined = virture_mysql:lookup(vmysql_test_player, [1]),
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
    virture_mysql:delete(vmysql_test_player, [2]),
    virture_mysql:insert(#vmysql_test_goods{player_id = 1, goods_id = 1, str = <<"11">>, to_str = <<"11">>, to_bin = <<"11">>}),
    virture_mysql:delete(vmysql_test_goods, [1, 2]),
    virture_mysql:insert(#vmysql_test_player{player_id = 3, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 3, goods_id = 1, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 3, goods_id = 2, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>}),
    #vmysql_test_player{str = <<"11">>} = virture_mysql:lookup(vmysql_test_player, [1]),
    undefined = virture_mysql:lookup(vmysql_test_player, [2]),
    #vmysql_test_player{str = <<"3">>} = virture_mysql:lookup(vmysql_test_player, [3]),
    #vmysql_test_goods{str = <<"11">>} = virture_mysql:lookup(vmysql_test_goods, [1, 1]),
    undefined = virture_mysql:lookup(vmysql_test_goods, [1, 2]),
    #vmysql_test_goods{str = <<"3">>} = virture_mysql:lookup(vmysql_test_goods, [3, 1]),
%%    io:format("~p~n", [get()]),
    virture_mysql:check_flush(),
    2 = ets:info(virture_mysql:make_ets_name(vmysql_test_player), size),
    5 = ets:info(virture_mysql:make_ets_name(vmysql_test_goods), size),
    #vmysql_test_player{str = <<"11">>} = virture_mysql:lookup(vmysql_test_player, [1]),
    undefined = virture_mysql:lookup(vmysql_test_player, [2]),
    #vmysql_test_player{str = <<"3">>} = virture_mysql:lookup(vmysql_test_player, [3]),
    #vmysql_test_goods{str = <<"11">>} = virture_mysql:lookup(vmysql_test_goods, [1, 1]),
    undefined = virture_mysql:lookup(vmysql_test_goods, [1, 2]),
    #vmysql_test_goods{str = <<"3">>} = virture_mysql:lookup(vmysql_test_goods, [3, 1]),
    %% hold
    Hold = virture_mysql:hold(),
    virture_mysql:insert(#vmysql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 1, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
    virture_mysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 2, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
    #vmysql_test_player{} = virture_mysql:lookup(vmysql_test_player, [4]),
    #vmysql_test_goods{} = virture_mysql:lookup(vmysql_test_goods, [4, 1]),
    #vmysql_test_goods{} = virture_mysql:lookup(vmysql_test_goods, [4, 2]),
    virture_mysql:rollback(Hold),
    2 = ets:info(virture_mysql:make_ets_name(vmysql_test_player), size),
    5 = ets:info(virture_mysql:make_ets_name(vmysql_test_goods), size),
    undefined = virture_mysql:lookup(vmysql_test_player, [4]),
    undefined = virture_mysql:lookup(vmysql_test_goods, [4, 1]),
    undefined = virture_mysql:lookup(vmysql_test_goods, [4, 2]),
    %% all_table
    [] = virture_mysql:all_table()--[vmysql_test_player, vmysql_test_goods],
    virture_mysql:clean(virture_mysql:all_table()),
    [] = virture_mysql:all_table(),
    %% flush
    virture_mysql:init(vmysql_test_player, [1]),
    virture_mysql:init(vmysql_test_goods, [1]),
    virture_mysql:insert(#vmysql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
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
    %% 初始化
    ets:delete_all_objects(virture_mysql:make_ets_name(vmysql_test_player)),
    erase(),
    %% dets
    virture_mysql:init(vmysql_test_player, []),
    %% 没有json, flush会失败, 会有一个正常报错
    virture_mysql:insert(#vmysql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
    flush(),
    flush_dets(),
    erase(),
    [] = ets:lookup(virture_mysql:make_ets_name(vmysql_test_player), [4]),
    fix_dets(undefined, fun(R) ->
        R1 = R#vmysql_test_player{to_json = [{1, {2, 3}}, {11, {22, 33}}]},
        virture_mysql:insert(R1)
                        end, undefined),
    [#vmysql_test_player{}] = ets:lookup(virture_mysql:make_ets_name(vmysql_test_player), [4]),
    ets:delete_all_objects(virture_mysql:make_ets_name(vmysql_test_player)),
    virture_mysql:init(vmysql_test_player, []),
    #vmysql_test_player{} = virture_mysql:lookup(vmysql_test_player, [4]),
    %% dirty lookup
    ets:delete_all_objects(virture_mysql:make_ets_name(vmysql_test_goods)),
    erase(),
    #vmysql_test_goods{} = virture_mysql:dirty_lookup(vmysql_test_goods, [4, 1]),
    %% fold_cache
    clean(),
    virture_mysql:init(vmysql_test_goods, [4]),
    2 = fold_cache(fun(_K, _V, Acc) ->
        Acc + 1
                   end, 0, vmysql_test_goods),
    virture_mysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 3, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
    3 = fold_cache(fun(_K, _V, Acc) ->
        Acc + 1
                   end, 0, vmysql_test_goods),
    %% 热更新定义, shell 执行
    %% vmysql_test_goods的to_str改成string类型
    %% os:cmd("rebar3 compile"), rr("include/*"), l(virture_mysql), l(virture_config).
    %% virture_mysql:hotfix(vmysql_test_goods, fun(R) -> virture_mysql:insert(R#vmysql_test_goods{to_str = <<"hotfix">>}) end).
    %% [#vmysql_test_goods{str = <<"hotfix">>}] = ets:lookup(virture_mysql:make_ets_name(vmysql_test_goods), [4, 1]).
    %% virture_mysql:flush().
    %% 看数据库
    %% virture_mysql:clean(), ets:delete_all_objects(virture_mysql:make_ets_name(vmysql_test_goods)).
    %% virture_mysql:init(vmysql_test_goods, [4]), virture_mysql:lookup(vmysql_test_goods, [4, 1]).
    {base_test, ok}.
