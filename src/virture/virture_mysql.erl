%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
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

-export([init_ets/0, lookup/2, insert/1, on_msg_end/0]).

-type field_type() :: int32|int64|uint32|uint64|float|string|to_string|binary|to_binary.
-export_type([field_type/0]).

%% @doc 初始化ets
init_ets() ->
    lists:foreach(fun(Virture) ->
        EtsName = make_ets_name(Virture),
        EtsName = ets:new(EtsName, Virture#vmysql.ets_opt)
                  end, virture_config:all(mysql)).

%% 查询一条数据
-spec lookup(atom(), list()) -> Record :: tuple()|undefined.
lookup(Table, Key) when is_list(Key) ->
    %% 先从变更的缓存中拿
    case get_from_change(Key, Table) of
        delete ->
            undefined;
        undefined ->
            %% 从缓存中拿
            get_from_cache(Key, Table);
        Record ->
            Record
    end;
lookup(Table, Key) ->
    lookup(Table, [Key]).

%% 先从变更的缓存中拿
get_from_change(Key, Table) ->
    case get(?PD_VMYSQL_CHANGE) of
        undefined ->
            put(?PD_VMYSQL_CHANGE, []),
            undefined;
        TableChangeList ->
            %% [{table, [#virture_change{}]}]
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
            end
    end.

%% 从缓存中拿
get_from_cache(Key, Table) ->
    case get({?PD_VMYSQL_CACHE, Table}) of
        undefined ->
            %% 初始化缓存
            Virture = init_virture(Key, virture_config:get(mysql, Table));
        Virture -> ok
    end,
    get_from_data(Key, Virture#vmysql.private_key_pos, Virture#vmysql.data).


%% 插入数据
insert(Record) ->
    Table = element(1, Record),
    case get({?PD_VMYSQL_CACHE, Table}) of
        undefined ->
            %% 初始化缓存
            #vmysql{
                state_pos = StatePos,
                private_key_pos = PrivateKeyPos,
                all_fields = AllField,
                private_key = PrivateKeyDef
            } = ConfigVirture = virture_config:get(mysql, Table),
            Key = make_private_key([lists:keyfind(Field, #vmysql_field.name, AllField) || Field <- PrivateKeyDef], Record),
            Virture = init_virture(Key, ConfigVirture),
            Record1 = setelement(StatePos, Record, ?VIRTURE_STATE_REPLACE),
            Record2 = setelement(PrivateKeyPos, Record1, Key),
            Change = insert_make_change(Key, Record2, ConfigVirture),
            put(?PD_VMYSQL_CHANGE, [Change | get(?PD_VMYSQL_CHANGE)]),
            insert_check_sync(1, Virture);
        Virture ->
            #vmysql{state_pos = StatePos, private_key_pos = PrivateKeyPos} = Virture,
            Record1 = setelement(StatePos, Record, ?VIRTURE_STATE_REPLACE),
            Key = element(PrivateKeyPos, Record),
            ChangeList = get(?PD_VMYSQL_CHANGE),
            %% 表是否有change缓存
            case lists:keytake(Table, #vmysql_change.table, ChangeList) of
                {value, #vmysql_change{data = Data} = Change, ChangeList1} ->
                    Data1 = set_to_data(Key, PrivateKeyPos, Record1, Data),
                    Change1 = Change#vmysql_change{data = Data1},
                    put(?PD_VMYSQL_CHANGE, [Change1 | ChangeList1]),
                    insert_check_sync(data_size(Data1), Virture);
                _ ->
                    ConfigVirture = virture_config:get(mysql, Table),
                    Record2 = setelement(PrivateKeyPos, Record1, Key),
                    Change = insert_make_change(Key, Record2, ConfigVirture),
                    put(?PD_VMYSQL_CHANGE, [Change | get(?PD_VMYSQL_CHANGE)]),
                    insert_check_sync(1, Virture)
            end
    end.

%% 构造change结构并且插入第一条数据
insert_make_change(Key, Record, ConfigVirture) ->
    #vmysql_change{
        table = ConfigVirture#vmysql.table,
        private_key_pos = ConfigVirture#vmysql.private_key_pos,
        state_pos = ConfigVirture#vmysql.state_pos,
        data = set_to_data(Key, ConfigVirture#vmysql.private_key_pos, Record, ConfigVirture#vmysql.data)
    }.

insert_check_sync(CurrentSize, #vmysql{table = Table, sync_size = SyncSize}) ->
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

on_msg_end() ->
    case get(?PD_VMYSQL_FLUSH) of
        List when is_list(List), List =/= [] ->
            flush_table(List, get(?PD_VMYSQL_CHANGE));
        _ ->
            skip
    end.

flush_table([], _ChangeList) ->
    ok;
flush_table([Table | T], ChangeList) ->
    {value, #vmysql_change{private_key_pos = PKPos, data = ChangeData}, ChangeList1} = lists:keytake(Table, #vmysql_change.table, ChangeList),
    %% 拿到cache
    #vmysql{
        data = Data, state_pos = StatePos,
        replace_sql = ReplaceSql, all_fields = AllFields
    } = Virture = get({?PD_VMYSQL_CACHE, Table}),
    {Data1, Sql} =
        fold_data(fun(Key, Value, {D, S} = Acc) ->
            case element(StatePos, Value) of
                ?VIRTURE_STATE_REPLACE ->
                    Value1 = setelement(StatePos, Value, ?VIRTURE_STATE_NOT_CHANGE),
                    D1 = set_to_data(Key, PKPos, Value1, D),
                    [[_, First] | Left] = [[$,, encode(Type, element(Pos, Value))] || #vmysql_field{type = Type, pos = Pos} <- AllFields],
                    S1 = [$,, $(, First, Left, $) | S],
                    {D1, S1};
                _ ->
                    Acc
            end
                  end, {Data, []}, PKPos, ChangeData),
    case Sql of
        [_ | Sql1] ->
            case mysql_poolboy:query(?VMYSQL_POOL, [ReplaceSql, Sql1]) of
                {error, Reason} ->
                    throw({mysql, Reason});
                _ ->
                    %% 保证报错的时候数据一致
                    Virture1 = Virture#vmysql{data = Data1},
                    put({?PD_VMYSQL_CACHE, Table}, Virture1),
                    put(?PD_VMYSQL_FLUSH, T),
                    put(?PD_VMYSQL_CHANGE, ChangeList1),
                    flush_table(T, ChangeList1)
            end;
        _ ->
            flush_table(T, ChangeList1)
    end.

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
            ?VIRTURE_LIST(Size + 1, [Value]);
        {value, _, List1} ->
            ?VIRTURE_LIST(Size, [Value | List1])
    end;
set_to_data(Key, _PrivateKeyPos, Value, Dict) ->
    dict:store(Key, Value, Dict).

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

%% ets名字
make_ets_name(Virture) ->
    list_to_atom("ets_" ++ atom_to_list(Virture#vmysql.table)).

%% 初始化本地缓存
init_virture(Key, #vmysql{table = Table, all_fields = AllField, private_key = PrivateKey, select_key = Selectkey} = Virture) ->
    %% 初始化优化sql
    Virture1 = Virture#vmysql{
        private_key = [lists:keyfind(Field, #vmysql_field.name, AllField) || Field <- PrivateKey],
        select_key = [lists:keyfind(Field, #vmysql_field.name, AllField) || Field <- Selectkey],
        where_sql = make_where_sql(Virture),
        select_sql = make_select_sql(Virture),
        replace_sql = make_replace_sql(Virture),
        delete_sql = make_delete_sql(Virture)
    },
    %% 初始化数据
    Virture2 = init_data(Key, Virture1),
    put({?PD_VMYSQL_CACHE, Table}, Virture2),
    case get(?PD_VMYSQL_CHANGE) of
        undefined -> put(?PD_VMYSQL_CHANGE, []);
        _ -> skip
    end,
    Virture2.

make_where_sql(Virture) ->
    case Virture#vmysql.private_key of
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

%% 初始化数据
init_data(Key, Virture) ->
    #vmysql{
        private_key_pos = PrivateKeyPos, state_pos = StatePos,
        private_key = PrivateKey, select_key = SelectKey,
        where_sql = WhereSql, select_sql = SelectSql,
        data = VData, init_fun = InitFun
    } = Virture,
    %% 先从ets拿
    EtsName = make_ets_name(Virture),
    KeySpec = make_ets_key_spec(Key, Virture),
    case ets:select(EtsName, [{{KeySpec, '$1'}, [], ['$1']}]) of
        [] ->
            %% 从数据库拿
            ValueList = [lists:nth(get_nth(Field, PrivateKey, 1), Key) || Field <- SelectKey],
            WhereSql1 = join_where(WhereSql, SelectKey, ValueList),
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
            fold_data(fun(K, Value, _Acc) ->
                ets:insert(EtsName, {K, Value})
                      end, [], PrivateKeyPos, Data),
            Virture#vmysql{data = Data};
        EtsData ->
            %% 构造本地缓存
            Data =
                lists:foldl(fun(Record, Acc) ->
                    set_to_data(element(PrivateKeyPos, Record), PrivateKeyPos, Record, Acc)
                            end, VData, EtsData),
            Virture#vmysql{data = Data}
    end.

make_private_key(PrivateKey, Record) ->
    [element(Pos, Record) || #vmysql_field{pos = Pos} <- PrivateKey].

make_ets_key_spec(Key, Virture) ->
    make_ets_key_spec(Key, Virture#vmysql.private_key, Virture#vmysql.select_key).

make_ets_key_spec([], [], _SelectKey) ->
    [];
make_ets_key_spec([Value | ValueT], [#vmysql_field{name = Name} | PKT], SelectKey) ->
    case lists:keymember(Name, #vmysql_field.name, SelectKey) of
        true ->
            [Value | make_ets_key_spec(ValueT, PKT, SelectKey)];
        _ ->
            ['_' | make_ets_key_spec(ValueT, PKT, SelectKey)]
    end.

get_nth(_Field, [], _N) ->
    erlang:error(bad_field);
get_nth(#vmysql_field{name = Find}, [#vmysql_field{name = Find} | _T], N) ->
    N;
get_nth(Field, [_ | T], N) ->
    get_nth(Field, T, N + 1).

join_where([], [], []) ->
    [];
join_where([SqlH | SqlT], [#vmysql_field{type = Type} | FieldT], [Value | ValueT]) ->
    [SqlH, encode(Type, Value) | join_where(SqlT, FieldT, ValueT)].

encode(to_string, Value) ->
    io_lib:format("~w", [Value]);
encode(to_binary, Value) ->
    erlang:term_to_binary(Value);
encode(_, Value) ->
    mysql_encode:encode(Value).

decode(to_string, Value) ->
    util:eval(Value);
decode(to_binary, Value) ->
    erlang:binary_to_term(Value);
decode(_, Value) ->
    Value.

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
