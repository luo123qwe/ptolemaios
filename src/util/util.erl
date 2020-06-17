%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 工具类
%%% @end
%%%-------------------------------------------------------------------
-module(util).
-author("dominic").

-include("util.hrl").

-export([list_fold/3, eval/1]).

-export([kv_get/3, kv_set/3]).

%% @doc 遍历列表</br>
%% 可返回?LIST_UTIL_BREAK|?LIST_UTIL_BREAK(Acc1)终止遍历
-spec list_fold(fun((E, Acc) -> Acc1|?UTIL_FOLD_BREAK|?UTIL_FOLD_BREAK(Acc1)), term(), list()) -> term()
    when E :: term(), Acc :: term(), Acc1 :: term().
list_fold(F, Acc, []) when is_function(F, 2) -> Acc;
list_fold(F, Acc, [H | T]) ->
    case F(H, Acc) of
        ?UTIL_FOLD_BREAK ->
            Acc;
        ?UTIL_FOLD_BREAK(Acc1) ->
            Acc1;
        Acc1 ->
            list_fold(F, Acc1, T)
    end.


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