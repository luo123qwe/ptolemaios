%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dynames_test).
-author("dominic").

-behaviour(dynames_unit).

-include("util.hrl").
-include("dynames.hrl").
-include("attr.hrl").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([init/2, filter_event_target/3, execute_event/4, init_svr/1]).

%% 没用到的, 初始化直接赋值
init(Unit, Dynames) ->
    {ok, Unit, Dynames}.

filter_event_target(Unit, Event, #dynames{unit_map = UnitMap}) ->
    #dynames_unit{id = UnitId} = Unit,
    %% 除自己以外的unit都是目标
    TargetMap =
        maps:filter(fun(K, _) ->
            UnitId =/= K
                    end, UnitMap),
    %% 再执行两次后停止, 模拟事件的数据传递
    case Event#dynames_event.stream of
        2 ->
            {ok, TargetMap, Event#dynames_event{stream = stop}};
        undefined ->
            {ok, TargetMap, Event#dynames_event{stream = 1}};
        Stream ->
            {ok, TargetMap, Event#dynames_event{stream = Stream + 1}}
    end.

execute_event(Target, Unit, Event, #dynames{frame = Frame, stream_event = StreamEvent} = State) ->
    ?debugFmt("~n~p ~p ~p ~p", [Target#dynames_unit.id, Unit#dynames_unit.id, Event#dynames_event.stream, dynames:rand()]),
    case Event#dynames_event.stream of
        stop ->
            %% 下一帧执行一个事件, 模拟事件延时触发
            NewEvent = dynames_event:copy(Frame + 1, Event#dynames_event.priority, Event),
            {ok, State#dynames{stream_event = dynames_event:insert_first(NewEvent#dynames_event{stream = undefined}, StreamEvent)}};
        _ ->
            %% 换成target发起, 模拟A事件触发B事件
            {ok, dynames_svr:execute_event(Event#dynames_event{user = Target#dynames_unit.id}, State)}
    end.

%% 手动测, Unit1加攻击, 攻击Unit2, 然后触发Unit1的死亡监听
init_svr(P) ->
    sys:replace_state(P, fun(_) ->
        Attr = [{?ATTR_HP, 100}, {?ATTR_ATTACK, 100}, {?ATTR_DEFENSE, 0}],
        BaseUnit1 = dynames_unit:new(1),
        Unit1 = BaseUnit1#dynames_unit{
            state = ?DYNAMES_UNIT_STATE_ALIVE,
            x = 0, y = 1000,
            attr = Attr, origin_attr = Attr
        },
        BaseUnit2 = dynames_unit:new(2),
        Unit2 = BaseUnit2#dynames_unit{
            state = ?DYNAMES_UNIT_STATE_ALIVE,
            x = 0, y = 0,
            attr = Attr, origin_attr = Attr
        },
        BaseSkill1Event = dynames_event:new(1, 1),
        Skill1Event = BaseSkill1Event#dynames_event{event = ?DYNAMES_EVENT_SKILL1(?DYNAMES_SKILL_ID2(1, 1)), user = Unit1#dynames_unit.id},
        BaseSkill2Event = dynames_event:new(1, 1),
        Skill2Event = BaseSkill2Event#dynames_event{event = ?DYNAMES_EVENT_SKILL1(?DYNAMES_SKILL_ID2(1, 2)), user = Unit1#dynames_unit.id},
        BaseDeadEvent = dynames_event:new(0, 1),
        DeadEvent = BaseDeadEvent#dynames_event{event = ?DYNAMES_EVENT_NORMAL_DEAD, user = Unit1#dynames_unit.id},
        #dynames{
            frame = 0,
            stream_event = #{1 => [Skill2Event], 2 => [Skill1Event]},
            trigger_event = #{?DYNAMES_ETTA_DEAD => [DeadEvent]},
            unit_map = #{
                Unit1#dynames_unit.id => Unit1,
                Unit2#dynames_unit.id => Unit2
            }
        }
                         end).


-ifdef(TEST).

base_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = dynames_svr:start_link([]),
            sys:replace_state(Pid, fun(_) ->
                BaseEvent1 = dynames_event:new(1, 1),
                Event1 = BaseEvent1#dynames_event{user = 1, event = ?DYNAMES_EVENT_TEST},
                BaseEvent2 = dynames_event:new(3, 1),
                Event2 = BaseEvent2#dynames_event{user = 2, event = ?DYNAMES_EVENT_TEST},
                #dynames{
                    stream_event = #{
                        1 => [Event1],
                        %% 这个事件不会执行
                        3 => [Event2]
                    },
                    unit_map = #{1 => #dynames_unit{id = 1, module = dynames_test}, 2 => #dynames_unit{id = 2, module = dynames_test}}
                }
                                   end),
            Pid
        end,
        fun(_) -> ok end,
        fun(Pid) ->
            ?_test(begin
                       Pid ! ?MSG_DYNAMES_NEXT_FRAME,
                       Pid ! ?MSG_DYNAMES_NEXT_FRAME
                   end)
        end
    }.

-endif.