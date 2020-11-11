%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 一个帧战斗框架
%%%
%%% 每帧处理一个事件列表
%%%
%%% 每个事件都有发起者和具体事件内容
%%%
%%% 事件处理流程: 筛选目标 -> 对每个目标执行该事件
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dynames_svr).
-author("dominic").

-behaviour(exia).

-include("dynames.hrl").
-include("util.hrl").

%% API
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([next_frame/1, execute_frame/1, execute_event/2]).

start_link(Args) ->
    exia:start_link(?MODULE, Args, []).

init(_Args) ->
    dynames:init_rand_seed(),
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
    case kv_op:lookup(Frame, StreamEvent, []) of
        [H | T] ->
            StreamEvent1 = ?IF(T =/= [], kv_op:store(Frame, T, StreamEvent), kv_op:delete(Frame, StreamEvent)),
            State1 = State#dynames{stream_event = StreamEvent1},
            %% 释放对象
            State2 = execute_event(H, State1),
            %% 执行效果
            execute_frame(State2);
        _ ->
            State
    end.

%% @doc 执行一个事件
-spec execute_event(#dynames_event{}, #dynames{}) -> #dynames{}.
execute_event(#dynames_event{user = User} = Event, #dynames{unit_map = UnitMap, event_deep = Deep} = State) ->
    #dynames_unit{module = Module} = Unit = kv_op:lookup(User, UnitMap, undefined),
    %% 事件深度
    %% todo 收集哪些信息?
    ?DO_IF(Deep > ?DYNAMES_EVENT_MAX_DEEP, exit(event_too_deep)),
    %% 过滤目标
    case filter_event_target(Unit, Event, State) of
        {ok, TargetMap, Event1} ->
            State1 = State#dynames{event_deep = Deep + 1},
            State2 =
                maps:fold(fun(_, Target, Acc) ->
                    try ?DYM_DYNAMES_UNIT3(Module, execute_event, [Target, Unit, Event1, Acc]) of
                        {ok, Acc1} ->
                            Acc1;
                        _ ->
                            Acc
                    catch
                        throw:?DYNAMES_RETURN1(?DYNAMES_RETURN_SKIP) ->
                            Acc;
                        throw:?DYNAMES_RETURN1({ok, #dynames{} = Acc1}) ->
                            Acc1;
                        C:E:S ->
                            %% TODO 报错处理方案??
                            erlang:raise(C, E, S)
                    end
                          end, State1, TargetMap),
            State2#dynames{event_deep = Deep};
        _ ->
            State
    end.

%% 过滤目标
filter_event_target(#dynames_unit{module = Module} = Unit, Event, State) ->
    try ?DYM_DYNAMES_UNIT3(Module, filter_event_target, [Unit, Event, State])
    catch
        throw:?DYNAMES_RETURN1(?DYNAMES_RETURN_SKIP) ->
            false;
        throw:?DYNAMES_RETURN1(Return) ->
            Return;
        C:E:S ->
            %% TODO 报错处理方案??
            erlang:raise(C, E, S)
    end;
filter_event_target(_, _, _) ->
    false.