%% @private
%%%-------------------------------------------------------------------
%%% @author Dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 前提
%%%     挂起再唤醒大量进程会不会出现卡顿
%%% 结论
%%%     1000个进程sleep100毫秒, 16毫秒左右
%%%     10000个进程sleep100毫秒, 120毫秒左右
%%%     100000个进程sleep100毫秒, 1100毫秒左右
%%%     修复函数耗时低, 可以满足需求
%%% @end
%%% Created : 03. 12月 2020 15:30
%%%-------------------------------------------------------------------
-module(perf_hotfix).
-author("Dominic").

-behaviour(exia).

-include("perf.hrl").
-include("util.hrl").

%% API
-export([run/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

run(#perf_hotfix{process_num = Num, sleep_time = Sleep} = Performance) ->
    %% 开进程
    PidList =
        lists:foldl(fun(N, Acc) ->
            {ok, Pid} = exia:start(?MODULE, [N], []),
            [Pid | Acc]
                    end, [], lists:seq(1, Num)),
    Start = erlang:system_time(millisecond),
    %% 挂起
    SuspendFailNum =
        lists:foldl(fun(Pid, Acc) ->
            case catch sys:suspend(Pid) of
                ok -> Acc;
                _ -> Acc + 1
            end
                    end, 0, PidList),
    %% sleep
    Self = self(),
    RefList =
        lists:foldl(fun(Pid, Acc) ->
            Ref = make_ref(),
            spawn(fun() ->
                sys:replace_state(Pid, fun(Id) -> timer:sleep(Sleep), Self ! Ref, Id + 1 end)
                  end),
            [Ref | Acc]
                    end, [], PidList),
    lists:foreach(fun(_) -> receive _ -> ok end end, RefList),
    %% 恢复
    ResumeFailNum =
        lists:foldl(fun(Pid, Acc) ->
            case catch sys:resume(Pid) of
                ok -> Acc;
                _ -> Acc + 1
            end
                    end, 0, PidList),
    End = erlang:system_time(millisecond),
    %% 清理
    lists:foreach(fun(Pid) ->
        exia:stop(Pid)
                  end, PidList),
    io:format("~n~wcost ~w, fail suspend ~w, fail resume ~w~n", [
        lists:zip(record_info(fields, perf_hotfix), tl(tuple_to_list(Performance))),
        End - Start, SuspendFailNum, ResumeFailNum
    ]).

init([Id]) ->
    {ok, Id}.

handle_call(Request, From, State) ->
    ?LOG_ERROR("unknow call from ~p~n~p", [Request, From]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOG_ERROR("unknow cast~n~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG_ERROR("unknow info~n~p", [Info]),
    {noreply, State}.