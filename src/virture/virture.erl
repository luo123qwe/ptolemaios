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
-module(virture).

-include("virture.hrl").

-export([init_ets/0, lookup/2]).

-type field_type() :: int32|int64|uint32|uint64|float|string|to_string|binary|to_binary.
-export_type([field_type/0]).

init_ets() ->
    lists:foreach(fun(Virture) ->
        create_table(),
        EtsName = make_ets_name(Virture),
        EtsName = ets:new(EtsName, [public, named_table])
                  end, virture_config:all()).

%% 查询一条数据
-spec lookup(atom(), list()) -> Record :: tuple()|undefined.
lookup(Table, Key) ->
    %% 先从变更的缓存中拿
    case get_from_cahnge(Key, Table) of
        delete ->
            undefined;
        undefined ->
            %% 从缓存中拿
            get_from_cache(Key, Table);
        Record ->
            Record
    end.

get_from_cahnge(Key, Table) ->
    case get(pd_virture_change) of
        undefined ->
            put(pd_virture_change, []),
            undefined;
        TableChangeList ->
            case lists:keyfind(Table, 1, TableChangeList) of
                false -> undefined;
                {_, ChangeList} ->
                    case lists:keyfind(Key, #virture_change.key, ChangeList) of
                        #virture_change{type = ?VIRTURE_STATE_DELETE} ->
                            delete;
                        #virture_change{type = ?VIRTURE_STATE_INSERT, record = Record} ->
                            Record;
                        _ ->
                            undefined
                    end
            end
    end.

get_from_cache(Key, Table) ->
    case get({pd_virture_cache, Table}) of
        undefined ->
            Virture = init_virture(Key, virture_config:get(Table));
        Virture -> ok
    end,
    get_from_data(Key, Virture#virture.data).

get_from_data(Key, List) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        false -> undefined;
        {_, Record} -> Record
    end;
get_from_data(Key, Dict) ->
        case dict:find(Key, Dict) of
        error -> undefined;
        {ok, Record} -> Record
    end.

set_to_data(Key, Value, List) when is_list(List) ->
    lists:keystore(Key, 1, List, {Key, Value});
set_to_data(Key, Value, Dict) ->
    dict:store(Key, Value, Dict).

fold_data(F, Acc, List) when is_list(List), is_function(F, 3) ->
    fold_data_list(F, Acc, List);
fold_data(F, Acc, Dict) ->
    dict:fold(F, Acc, Dict).

fold_data_list(_F, Acc, []) ->
    Acc;
fold_data_list(F, Acc, [{K, V} | T]) ->
    fold_data_list(F, F(K, V, Acc), T).

make_ets_name(Virture) ->
    list_to_atom("ets_" ++ atom_to_list(Virture#virture.table)).

init_virture(Key, #virture{table = Table, all_fields = AllField} = Virture) ->
    %% 初始化优化sql
    Virture1 = Virture#virture{
        private_key = [lists:keyfind(Field, #virture_field.name, AllField) || Field <- Virture#virture.private_key],
        select_key = [lists:keyfind(Field, #virture_field.name, AllField) || Field <- Virture#virture.select_key],
        where_sql = make_where_sql(Virture),
        select_sql = make_select_sql(Virture),
        insert_sql = make_insert_sql(Virture),
        update_sql = make_update_sql(Virture),
        delete_sql = make_delete_sql(Virture)
    },
    Virture2 = init_data(Key, Virture1),
    put({pd_virture_cache, Table}, Virture2),
    Virture2.

make_where_sql(Virture) ->
    case Virture#virture.private_key of
        [H] ->
            [" WHERE " ++ atom_to_list(H) ++ "="];
        [H | T] ->
            HSql = " WHERE " ++ atom_to_list(H) ++ "=",
            TSql = [" AND " ++ atom_to_list(Name) ++ "=" || Name <- T],
            [HSql | TSql]
    end.

make_select_sql(Virture) ->
    AllField = string:join([atom_to_list(VK#virture_field.name) || VK <- Virture#virture.all_fields], ","),
    "SELECT " ++ AllField ++ " FROM " ++ atom_to_list(Virture#virture.table).

make_insert_sql(Virture) ->
    AllField = string:join([atom_to_list(VK#virture_field.name) || VK <- Virture#virture.all_fields], ","),
    ["INSERT " ++ atom_to_list(Virture#virture.table) ++ " (" ++ AllField ++ ")VALUES"].

make_update_sql(Virture) ->
    "UPDATE " ++ atom_to_list(Virture#virture.table) ++ " SET ".

make_delete_sql(Virture) ->
    "DELETE FROM " ++ atom_to_list(Virture#virture.table).

init_data(Key, Virture) ->
    #virture{
        private_key = PrivateKey, select_key = SelectKey,
        where_sql = WhereSql, select_sql = SelectSql,
        data = VData, init_fun = InitFun
    } = Virture,
    EtsName = make_ets_name(Virture),
    KeySpec = make_ets_key_spec(Key, Virture),
    case ets:select(EtsName, [{{KeySpec, '$1'}, [], ['$1']}]) of
        [] ->
            ValueList = [lists:nth(get_nth(Field, PrivateKey, 1), Key) || Field <- SelectKey],
            WhereSql1 = join_where(WhereSql, SelectKey, ValueList),
            {ok, _ColumnNames, Rows} = mysql_poolboy:query(virture, [SelectSql, WhereSql1]),
            Data =
                lists:foldl(fun(Row, Acc) ->
                    Record = init_record(InitFun, Row, Virture),
                    K = [element(Pos, Record) || #virture_field{pos = Pos} <- PrivateKey],
                    set_to_data(K, Record, Acc)
                            end, VData, Rows),
            %% 全部初始化成功再放到ets
            fold_data(fun(K, Value, _Acc) ->
                ets:insert(EtsName, {K, Value})
                      end, Virture, Data),
            Virture#virture{data = Data};
        EtsData ->
            Data =
                lists:foldl(fun(Record, Acc) ->
                    K = [element(Pos, Record) || #virture_field{pos = Pos} <- PrivateKey],
                    set_to_data(K, Record, Acc)
                            end, VData, EtsData),
            Virture#virture{data = Data}
    end.

make_ets_key_spec(Key, Virture) ->
    make_ets_key_spec(Key, Virture#virture.private_key, Virture#virture.select_key).

make_ets_key_spec([], [], _SelectKey) ->
    [];
make_ets_key_spec([Value | ValueT], [#virture_field{name = Name} | PKT], SelectKey) ->
    case lists:keymember(Name, #virture_field.name, SelectKey) of
        true ->
            [Value | make_ets_key_spec(ValueT, PKT, SelectKey)];
        _ ->
            ['_' | make_ets_key_spec(ValueT, PKT, SelectKey)]
    end.

get_nth(_Field, [], _N) ->
    erlang:error(bad_field);
get_nth(#virture_field{name = Find}, [#virture_field{name = Find} | _T], N) ->
    N;
get_nth(Field, [_ | T], N) ->
    get_nth(Field, T, N + 1).

join_where([], [], []) ->
    [];
join_where([SqlH | SqlT], [#virture_field{type = Type} | FieldT], [Value | ValueT]) ->
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

init_record(Init, Row, Virture) ->
    BaseRecord = erlang:make_tuple(Virture#virture.record_size, undefined, [{1, Virture#virture.table}]),
    init_record(Init, Row, Virture#virture.all_fields, BaseRecord).

init_record(InitFun, [], [], Record) ->
    case InitFun of
        {M, F} -> M:F(Record);
        _ -> Record
    end;
init_record(InitFun, [Value | ValueT], [#virture_field{pos = Pos, type = Type} | FieldT], Record) ->
    init_record(InitFun, ValueT, FieldT, setelement(Pos, Record, decode(Type, Value))).
