%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 工具杂项
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(util).
-author("dominic").

-include("util.hrl").

-export([eval/1]).

-export([is_list_intersect/2, is_list_contain/2]).


%% @doc 执行一条erlang语句
-spec eval(string()) -> term().
eval(Str) when is_binary(Str) ->
    eval(binary_to_list(Str));
eval(Str) when is_list(Str) ->
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

%% @doc 列表1与列表2是否有交集
is_list_intersect(List1, List2) ->
    lists:any(fun(H1) ->
        lists:member(H1, List1)
              end, List2).

%% @doc 列表1是否包含列表2
is_list_contain(List1, List2) ->
    lists:all(fun(H1) ->
        lists:member(H1, List1)
              end, List2).



