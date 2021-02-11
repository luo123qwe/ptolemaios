%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(battle_test).
-author("dominic").

-behaviour(battle_unit).

-include("plm_lib.hrl").
-include("battle.hrl").
-include("attr.hrl").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([init/2, filter_event_target/3, execute_event/4, init_svr/1]).

%% 没用到的, 初始化直接赋值
init(Unit, Battle) ->
    {ok, Unit, Battle}.

filter_event_target(Unit, Event, #battle{unit_map = UnitMap}) ->
    #battle_unit{id = UnitId} = Unit,
    %% 除自己以外的unit都是目标
    TargetMap =
        maps:filter(fun(K, _) ->
            UnitId =/= K
                    end, UnitMap),
    %% 再执行两次后停止, 模拟事件的数据传递
    case Event#battle_event.stream of
        2 ->
            {ok, TargetMap, Event#battle_event{stream = stop}};
        undefined ->
            {ok, TargetMap, Event#battle_event{stream = 1}};
        Stream ->
            {ok, TargetMap, Event#battle_event{stream = Stream + 1}}
    end.

execute_event(Target, Unit, Event, #battle{frame = Frame, stream_event = StreamEvent} = State) ->
    ?debugFmt("~n~p ~p ~p ~p", [Target#battle_unit.id, Unit#battle_unit.id, Event#battle_event.stream, battle:rand()]),
    case Event#battle_event.stream of
        stop ->
            %% 下一帧执行一个事件, 模拟事件延时触发
            NewEvent = battle_event:copy(Frame + 1, Event#battle_event.priority, Event),
            {ok, State#battle{stream_event = battle_event:insert_first(NewEvent#battle_event{stream = undefined}, StreamEvent)}};
        _ ->
            %% 换成target发起, 模拟A事件触发B事件
            {ok, battle_svr:execute_event(Event#battle_event{user = Target#battle_unit.id}, State)}
    end.

%% 手动测, Unit1加攻击, 攻击Unit2, 然后触发Unit1的死亡监听
init_svr(P) ->
    sys:replace_state(P, fun(_) ->
        Attr = [{?ATTR_HP, 100}, {?ATTR_ATTACK, 100}, {?ATTR_DEFENSE, 0}],
        BaseUnit1 = battle_unit:new(1),
        Unit1 = BaseUnit1#battle_unit{
            state = ?M14_UNIT_STATE_ALIVE,
            x = 0, y = 1000,
            attr = Attr, origin_attr = Attr
        },
        BaseUnit2 = battle_unit:new(2),
        Unit2 = BaseUnit2#battle_unit{
            state = ?M14_UNIT_STATE_ALIVE,
            x = 0, y = 0,
            attr = Attr, origin_attr = Attr
        },
        BaseSkill1Event = battle_event:new(1, 1),
        Skill1Event = BaseSkill1Event#battle_event{event = ?M14_E_SKILL1(?M14_SKILL_ID2(1, 1)), user = Unit1#battle_unit.id},
        BaseSkill2Event = battle_event:new(1, 1),
        Skill2Event = BaseSkill2Event#battle_event{event = ?M14_E_SKILL1(?M14_SKILL_ID2(1, 2)), user = Unit1#battle_unit.id},
        BaseDeadEvent = battle_event:new(0, 1),
        DeadEvent = BaseDeadEvent#battle_event{event = ?M14_E_NORMAL_DEAD, user = Unit1#battle_unit.id},
        #battle{
            frame = 0,
            stream_event = #{1 => [Skill2Event], 2 => [Skill1Event]},
            trigger_event = #{?M14_ETTA_DEAD => [DeadEvent]},
            unit_map = #{
                Unit1#battle_unit.id => Unit1,
                Unit2#battle_unit.id => Unit2
            }
        }
                         end).


-ifdef(TEST).

base_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = battle_svr:start_link([]),
            sys:replace_state(Pid, fun(_) ->
                BaseEvent1 = battle_event:new(1, 1),
                Event1 = BaseEvent1#battle_event{user = 1, event = ?M14_E_TEST},
                BaseEvent2 = battle_event:new(3, 1),
                Event2 = BaseEvent2#battle_event{user = 2, event = ?M14_E_TEST},
                #battle{
                    stream_event = #{
                        1 => [Event1],
                        %% 这个事件不会执行
                        3 => [Event2]
                    },
                    unit_map = #{1 => #battle_unit{id = 1, module = battle_test}, 2 => #battle_unit{id = 2, module = battle_test}}
                }
                                   end),
            Pid
        end,
        fun(_) -> ok end,
        fun(Pid) ->
            ?_test(begin
                       Pid ! ?MSG14_NEXT_FRAME,
                       Pid ! ?MSG14_NEXT_FRAME
                   end)
        end
    }.

-endif.