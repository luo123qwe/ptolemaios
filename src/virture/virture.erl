%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 数据缓存和落地
%%% @end
%%%-------------------------------------------------------------------
-module(virture).

-behaviour(gen_server).

-include("virture.hrl").

-export([init_ets/0, get_spec/0, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

%% @doc 初始化ets
-spec init_ets() -> ok.
init_ets() ->
    init_ets(virture_define:all()).

init_ets([]) ->
    ok;
init_ets([H | T]) ->
    Name = "ets_virture_" ++ H#virture.table,
    ets:new(list_to_atom(Name), [public, named_table, {keypos, H#virture.ets_key_pos}, {write_concurrency, true}]),
    init_ets(T).

%% @doc 获取监督树spec
-spec get_spec() -> [supervisor:child_spec()].
get_spec() ->
    get_spec(virture_define:all(), []).

get_spec([], SpecList) ->
    SpecList;
get_spec([H | T], SpecList) ->
    BaseName = "virture_" ++ H#virture.table,
    lists:foldl(fun(N, Acc) ->
        Name = list_to_atom(BaseName ++ "_" ++ integer_to_list(N)),
        [#{
            id => Name, start => {?MODULE, start_link, [N, H]},
            restart => permanent, shutdown => 5000, type => worker,
            modules => [?MODULE]
        } | Acc] end, SpecList, lists:seq(1, H#virture.server_num)),
    get_spec(T, SpecList).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Index, Virture) ->
    BaseName = "virture_" ++ Virture#virture.table,
    Name = list_to_atom(BaseName ++ "_" ++ integer_to_list(Index)),
    gen_server:start_link(Name, ?MODULE, [Index, Virture], []).

init([Index, Virture]) ->
    {ok, #virture_state{index = Index, virture = init_sql(Virture)}}.

handle_call(_Request, _From, State = #virture_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #virture_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #virture_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #virture_state{}) ->
    ok.

code_change(_OldVsn, State = #virture_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_sql(Virture) ->
    FList = [fun init_select_sql/1, fun init_insert_sql/1, fun init_update_sql/1, fun init_delete_sql/1],
    lists:foldl(fun(F, Acc) ->
        F(Acc)
                end, Virture, FList).

init_select_sql(Virture) ->
    case Virture#virture.select_fun of
        {_Module, _Function} ->
            Virture;
        _ ->
            %% ["SELECT f1,f2 FROM t WHERE k1="," AND k2=",,,]
            [WhereH | WhereT] = init_where(Virture#virture.key),
            Sql = [lists:flatten(["SELECT ", implode_field(Virture#virture.fields, ","), " FROM ",
                atom_to_list(Virture#virture.table), " WHERE ", WhereH]) | WhereT],
            Virture#virture{select_fun = Sql}
    end.

init_insert_sql(Virture) ->
    case Virture#virture.insert_fun of
        {_Module, _Function} ->
            Virture#virture.insert_fun;
        _ ->
            %% ["INSERT INTO t (k1,k2,f1,f2)VALUES"]
            Sql = [lists:flatten(["INSERT INTO ", atom_to_list(Virture#virture.table), " (",
                implode_field(Virture#virture.key ++ Virture#virture.fields, ","), ")VALUES"])],
            Virture#virture{insert_fun = Sql}
    end.

init_update_sql(Virture) ->
    case Virture#virture.update_fun of
        {_Module, _Function} ->
            Virture;
        _ ->
            %% ["UPDATE t SET f1=",",f2="," WHERE k1="," AND k2="]
            [FeildH | FeildT] = Virture#virture.fields,
            FeildT1 = ["," ++ atom_to_list(Field) ++ "=" || {Field, _, _} <- FeildT],
            [WhereH | WhereT] = init_where(Virture#virture.key),
            Sql = [lists:flatten(["UPDATE ", atom_to_list(Virture#virture.table), " SET ",
                atom_to_list(element(1, FeildH)), "="])] ++ FeildT1 ++ [" WHERE " ++ WhereH | WhereT],
            Virture#virture{update_fun = Sql}
    end.

init_delete_sql(Virture) ->
    case Virture#virture.delete_fun of
        {_Module, _Function} ->
            Virture;
        _ ->
            %% ["DELETE FROM t WHERE k1="," AND k2="]
            [WhereH | WhereT] = init_where(Virture#virture.key),
            Sql = [lists:flatten(["DELETE FROM ", atom_to_list(Virture#virture.table), " WHERE ", WhereH]) | WhereT],
            Virture#virture{delete_fun = Sql}
    end.

init_where([{Field, _Index, _Type} | T]) ->
    [atom_to_list(Field) ++ "=" | init_where_1(T)].

init_where_1([]) ->
    [];
init_where_1([{Field, _Index, _Type} | T]) ->
    [" AND " ++ atom_to_list(Field) ++ "=" | init_where_1(T)].

make_sql(PrepareSql, [], Fields2, Record) ->
    make_sql(PrepareSql, Fields2, Record);
make_sql([H | T], [FieldH | FieldT], Fields2, Record) ->
    [H, make_field(FieldH, Record) | make_sql(T, FieldT, Fields2, Record)].

make_sql([], [], _Record) ->
    [$;];
make_sql([H | T], [FieldH | FieldT], Record) ->
    [H, make_field(FieldH, Record) | make_sql(T, FieldT, Record)].

make_field({_Field, Index, Type}, Record) ->
    Value = element(Index, Record),
    case Type of
        string ->
            mysql_encode:encode(io_lib:format("~w", [Value]));
        binary ->
            mysql_encode:encode(erlang:term_to_binary(Value));
        _ ->
            mysql_encode:encode(Value)
    end.

make_update_field([], _Record) ->
    [];
make_update_field([H | T], Record) ->
    [make_field(H, Record), $, | make_update_field(T, Record)].

implode_field([{Field, _Index, _Type}], _SepH) ->
    [atom_to_list(Field)];
implode_field([{Field, _Index, _Type} | T], Sep) ->
    [atom_to_list(Field) ++ Sep | implode_field(T, Sep)].
