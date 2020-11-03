%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 一个帧战斗框架
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dynames_svr).
-author("dominic").

-behaviour(exia).

-include("dynames.hrl").
-include("util.hrl").

%% API
-export([start/2, start/3, start_link/2, start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([next_frame/1, execute_frame/1, execute_event/2]).

start(Args, Options) ->
    exia:start(?MODULE, Args, Options).

start(Name, Args, Options) ->
    exia:start(Name, ?MODULE, Args, Options).

start_link(Args, Options) ->
    exia:start_link(?MODULE, Args, Options).

start_link(Name, Args, Options) ->
    exia:start_link(Name, ?MODULE, Args, Options).

init(_Args) ->
    {ok, #dynames{}}.

handle_call(Request, From, State) ->
    ?LOG_ERROR("unknow call from ~p~n~p", [Request, From]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOG_ERROR("unknow cast~n~p", [Request]),
    {noreply, State}.

handle_info(?MSG_DYNAMES_NEXT_FRAME, State) ->
    State1 = next_frame(State),
    {noreply, State1};
handle_info(Info, State) ->
    ?LOG_ERROR("unknow info~n~p", [Info]),
    {noreply, State}.

%% @doc 下一帧
-spec next_frame(#dynames{}) -> #dynames{}.
next_frame(State) ->
    State1 = State#dynames{frame = State#dynames.frame + 1},
    execute_frame(State1).

%% @doc 执行一帧
-spec execute_frame(#dynames{}) -> #dynames{}.
execute_frame(#dynames{frame = Frame, stream_event = StreamEvent} = State) ->
    case StreamEvent of
        [#dynames_event{frame = ExecuteFrame} = H | T] when ExecuteFrame =< Frame ->
            State1 = State#dynames{stream_event = T},
            %% 释放对象
            State2 = execute_event(H, State1),
            %% 执行效果
            execute_frame(State2);
        _ ->
            State
    end.

%% @doc 执行一个事件
-spec execute_event(#dynames_event{}, #dynames{}) -> #dynames{}.
execute_event(#dynames_event{user = User} = Event, #dynames{unit_map = UnitMap} = State) ->
    #dynames_unit{module = Module} = Unit = kv_op:lookup(User, UnitMap, undefined),
    %% 过滤目标
    case filter_event_target(Unit, Event, State) of
        {ok, TargetList, Event1} ->
            %% 执行事件
            case ?DYM_DYNAMES_UNIT3(Module, execute_event, [TargetList, Unit, Event1, State]) of
                {ok, State1} ->
                    State1;
                _ ->
                    State
            end;
        _ ->
            State
    end.

%% 过滤目标
filter_event_target(#dynames_unit{module = Module} = Unit, Event, State) ->
    ?DYM_DYNAMES_UNIT3(Module, filter_event_target, [Unit, Event, State]);
filter_event_target(_, _, _) ->
    false.