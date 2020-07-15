%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 杂七杂八的丢这里
%%% @end
%%%-------------------------------------------------------------------
-module(util).
-author("dominic").

-include("util.hrl").

-export([eval/1, current_log_index/0]).

-export([kv_get/3, kv_set/3]).

%% @doc 获取kv结构的值
-spec kv_get(term(), [{Key :: term(), Value :: term()}]|map()|dict:dict()|gb_trees:tree()|ets:tab(), term()) -> term().
kv_get(Key, List, Default) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end;
kv_get(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
kv_get(Key, Dict, Default) when is_tuple(Dict), element(1, Dict) == dict ->
    case dict:find(Key, Dict) of
        {ok, Value} -> Value;
        error -> Default
    end;
kv_get(Key, {_, _} = GBTree, Default) ->
    case gb_trees:lookup(Key, GBTree) of
        {value, Value} -> Value;
        none -> Default
    end;
kv_get(Key, Ets, Default) when is_atom(Ets); is_reference(Ets) ->
    case ets:lookup(Ets, Key) of
        [] -> Default;
        [Value] -> Value
    end.


%% @doc 设置kv结构的值
-spec kv_set(term(), term(), [{Key :: term(), Value :: term()}]|map()|dict:dict()|gb_trees:tree()|ets:tab()) -> term().
kv_set(Key, Value, List) when is_list(List) ->
    lists:keystore(Key, 1, List, {Key, Value});
kv_set(Key, Value, Map) when is_map(Map) ->
    maps:put(Key, Value, Map);
kv_set(Key, Value, Dict) when is_tuple(Dict), element(1, Dict) == dict ->
    dict:store(Key, Value, Dict);
kv_set(Key, Value, {_, _} = GBTree) ->
    gb_trees:insert(Key, Value, GBTree);
kv_set(_Key, Value, Ets) when is_atom(Ets); is_reference(Ets) ->
    ets:insert(Ets, Value).


%% @doc 执行一条erlang语句
-spec eval(string()) -> term().
eval(Str) ->
    {ok, Tokens, _} = erl_scan:string(eval_fix_string(Str)),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Exprs, []),
    Value.

eval_fix_string([]) ->
    [$.];
eval_fix_string([$.]) ->
    [$.];
eval_fix_string([H | T]) ->
    [H | eval_fix_string(T)].

current_log_index()->
    case disk_log:info(ptolemaios) of
        {error, _} ->
            disk_log_down;
        Info ->
            element(2, lists:keyfind(current_file, 1, Info))
    end.