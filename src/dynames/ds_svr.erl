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
-module(ds_svr).
-author("dominic").

-behaviour(exia).

-include("ds.hrl").
-include("ptolemaios_lib.hrl").

%% API
-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([next_frame/1, execute_frame/1, execute_event/2]).

start_link(Args) ->
    exia:start_link(?MODULE, Args, []).

init(_Args) ->
    ds:init_rand_seed(),
    {ok, #ds{}}.

handle_call(Request, From, State) ->
    ?LOG_ERROR("unknow call from ~p~n~p", [Request, From]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOG_ERROR("unknow cast~n~p", [Request]),
    {noreply, State}.

handle_info(?MSG_DS_NEXT_FRAME, State) ->
    State1 = next_frame(State),
    {noreply, State1};
handle_info(Info, State) ->
    ?LOG_ERROR("unknow info~n~p", [Info]),
    {noreply, State}.

%% @doc 下一帧
-spec next_frame(#ds{}) -> #ds{}.
next_frame(State) ->
    State1 = State#ds{frame = State#ds.frame + 1},
    execute_frame(State1).

%% @doc 执行一帧
-spec execute_frame(#ds{}) -> #ds{}.
execute_frame(#ds{frame = Frame, stream_event = StreamEvent} = State) ->
    case ptolemaios_kv:lookup(Frame, StreamEvent, []) of
        [H | T] ->
            StreamEvent1 = ?IF(T =/= [], ptolemaios_kv:store(Frame, T, StreamEvent), ptolemaios_kv:delete(Frame, StreamEvent)),
            State1 = State#ds{stream_event = StreamEvent1},
            %% 释放对象
            State2 = execute_event(H, State1),
            %% 执行效果
            execute_frame(State2);
        _ ->
            State
    end.

%% @doc 执行一个事件
-spec execute_event(#ds_event{}, #ds{}) -> #ds{}.
execute_event(#ds_event{user = User} = Event, #ds{unit_map = UnitMap, event_deep = Deep} = State) ->
    case ptolemaios_kv:lookup(User, UnitMap, undefined) of
        undefined ->% 被移出出游戏了
            State;
        #ds_u{module = Module} = Unit ->
            %% 事件深度
            %% todo 收集哪些信息?
            ?DO_IF(Deep > ?DS_EXECUTE_EVENT_MAX_DEEP, exit(event_too_deep)),
            %% 过滤目标
            case filter_event_target(Unit, Event, State) of
                {ok, TargetMap, Event1} ->
                    State1 = State#ds{event_deep = Deep + 1},
                    State2 =
                        maps:fold(fun(_, Target, Acc) ->
                            try ?DYM_DS_UNIT3(Module, execute_event, [Target, Unit, Event1, Acc]) of
                                {ok, Acc1} ->
                                    Acc1;
                                _ ->
                                    Acc
                            catch
                                throw:?DS_R1(?DS_R_SKIP) ->
                                    Acc;
                                throw:?DS_R1({ok, #ds{} = Acc1}) ->
                                    Acc1;
                                C:E:S ->
                                    %% TODO 报错处理方案??
                                    erlang:raise(C, E, S)
                            end
                                  end, State1, TargetMap),
                    State2#ds{event_deep = Deep};
                _ ->
                    State
            end
    end.

%% 过滤目标
filter_event_target(#ds_u{module = Module} = Unit, Event, State) ->
    try ?DYM_DS_UNIT3(Module, filter_event_target, [Unit, Event, State])
    catch
        throw:?DS_R1(?DS_R_SKIP) ->
            false;
        throw:?DS_R1(Return) ->
            Return;
        C:E:S ->
            %% TODO 报错处理方案??
            erlang:raise(C, E, S)
    end;
filter_event_target(_, _, _) ->
    false.