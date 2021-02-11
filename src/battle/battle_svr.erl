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
-module(battle_svr).
-author("dominic").

-behaviour(plm_svr).

-include("battle.hrl").
-include("plm_lib.hrl").

%% API
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([next_frame/1, execute_frame/1, execute_event/2]).

start_link(Args) ->
    plm_svr:start_link(?MODULE, Args, []).

init(_Args) ->
    battle:init_rand_seed(),
    {ok, #battle{}}.

handle_call(Request, From, State) ->
    ?LOG_ERROR("unknow call from ~p~n~p", [Request, From]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOG_ERROR("unknow cast~n~p", [Request]),
    {noreply, State}.

handle_info(?MSG14_NEXT_FRAME, State) ->
    State1 = next_frame(State),
    {noreply, State1};
handle_info(Info, State) ->
    ?LOG_ERROR("unknow info~n~p", [Info]),
    {noreply, State}.

%% @doc 下一帧
-spec next_frame(#battle{}) -> #battle{}.
next_frame(State) ->
    State1 = State#battle{frame = State#battle.frame + 1},
    execute_frame(State1).

%% @doc 执行一帧
-spec execute_frame(#battle{}) -> #battle{}.
execute_frame(#battle{frame = Frame, stream_event = StreamEvent} = State) ->
    case plm_kv:lookup(Frame, StreamEvent, []) of
        [H | T] ->
            StreamEvent1 = ?IF(T =/= [], plm_kv:store(Frame, T, StreamEvent), plm_kv:delete(Frame, StreamEvent)),
            State1 = State#battle{stream_event = StreamEvent1},
            %% 释放对象
            State2 = execute_event(H, State1),
            %% 执行效果
            execute_frame(State2);
        _ ->
            State
    end.

%% @doc 执行一个事件
-spec execute_event(#battle_event{}, #battle{}) -> #battle{}.
execute_event(#battle_event{user = User} = Event, #battle{unit_map = UnitMap, event_deep = Deep} = State) ->
    case plm_kv:lookup(User, UnitMap, undefined) of
        undefined ->% 被移出出游戏了
            State;
        #battle_unit{module = Module} = Unit ->
            %% 事件深度
            %% todo 收集哪些信息?
            ?DO_IF(Deep > ?M14_EXECUTE_EVENT_MAX_DEEP, exit(event_too_deep)),
            %% 过滤目标
            case filter_event_target(Unit, Event, State) of
                {ok, TargetMap, Event1} ->
                    State1 = State#battle{event_deep = Deep + 1},
                    State2 =
                        maps:fold(fun(_, Target, Acc) ->
                            try ?DYM14_UNIT3(Module, execute_event, [Target, Unit, Event1, Acc]) of
                                {ok, Acc1} ->
                                    Acc1;
                                _ ->
                                    Acc
                            catch
                                throw:?M14_R1(?M14_R_SKIP) ->
                                    Acc;
                                throw:?M14_R1({ok, #battle{} = Acc1}) ->
                                    Acc1;
                                C:E:S ->
                                    %% TODO 报错处理方案??
                                    erlang:raise(C, E, S)
                            end
                                  end, State1, TargetMap),
                    State2#battle{event_deep = Deep};
                _ ->
                    State
            end
    end.

%% 过滤目标
filter_event_target(#battle_unit{module = Module} = Unit, Event, State) ->
    try ?DYM14_UNIT3(Module, filter_event_target, [Unit, Event, State])
    catch
        throw:?M14_R1(?M14_R_SKIP) ->
            false;
        throw:?M14_R1(Return) ->
            Return;
        C:E:S ->
            %% TODO 报错处理方案??
            erlang:raise(C, E, S)
    end;
filter_event_target(_, _, _) ->
    false.