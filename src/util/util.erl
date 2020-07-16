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

%% @doc 当前日志索引
current_log_index()->
    case disk_log:info(ptolemaios) of
        {error, _} ->
            disk_log_down;
        Info ->
            element(2, lists:keyfind(current_file, 1, Info))
    end.