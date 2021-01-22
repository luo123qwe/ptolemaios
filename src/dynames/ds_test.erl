%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ds_test).
-author("dominic").

-behaviour(ds_unit).

-include("ptolemaios_lib.hrl").
-include("ds.hrl").
-include("attr.hrl").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([init/2, filter_event_target/3, execute_event/4, init_svr/1]).

%% 没用到的, 初始化直接赋值
init(Unit, Dynames) ->
    {ok, Unit, Dynames}.

filter_event_target(Unit, Event, #ds{unit_map = UnitMap}) ->
    #ds_u{id = UnitId} = Unit,
    %% 除自己以外的unit都是目标
    TargetMap =
        maps:filter(fun(K, _) ->
            UnitId =/= K
                    end, UnitMap),
    %% 再执行两次后停止, 模拟事件的数据传递
    case Event#ds_event.stream of
        2 ->
            {ok, TargetMap, Event#ds_event{stream = stop}};
        undefined ->
            {ok, TargetMap, Event#ds_event{stream = 1}};
        Stream ->
            {ok, TargetMap, Event#ds_event{stream = Stream + 1}}
    end.

execute_event(Target, Unit, Event, #ds{frame = Frame, stream_event = StreamEvent} = State) ->
    ?debugFmt("~n~p ~p ~p ~p", [Target#ds_u.id, Unit#ds_u.id, Event#ds_event.stream, ds:rand()]),
    case Event#ds_event.stream of
        stop ->
            %% 下一帧执行一个事件, 模拟事件延时触发
            NewEvent = ds_event:copy(Frame + 1, Event#ds_event.priority, Event),
            {ok, State#ds{stream_event = ds_event:insert_first(NewEvent#ds_event{stream = undefined}, StreamEvent)}};
        _ ->
            %% 换成target发起, 模拟A事件触发B事件
            {ok, ds_svr:execute_event(Event#ds_event{user = Target#ds_u.id}, State)}
    end.

%% 手动测, Unit1加攻击, 攻击Unit2, 然后触发Unit1的死亡监听
init_svr(P) ->
    sys:replace_state(P, fun(_) ->
        Attr = [{?ATTR_HP, 100}, {?ATTR_ATTACK, 100}, {?ATTR_DEFENSE, 0}],
        BaseUnit1 = ds_unit:new(1),
        Unit1 = BaseUnit1#ds_u{
            state = ?DS_UNIT_STATE_ALIVE,
            x = 0, y = 1000,
            attr = Attr, origin_attr = Attr
        },
        BaseUnit2 = ds_unit:new(2),
        Unit2 = BaseUnit2#ds_u{
            state = ?DS_UNIT_STATE_ALIVE,
            x = 0, y = 0,
            attr = Attr, origin_attr = Attr
        },
        BaseSkill1Event = ds_event:new(1, 1),
        Skill1Event = BaseSkill1Event#ds_event{event = ?DS_E_SKILL1(?DS_SKILL_ID2(1, 1)), user = Unit1#ds_u.id},
        BaseSkill2Event = ds_event:new(1, 1),
        Skill2Event = BaseSkill2Event#ds_event{event = ?DS_E_SKILL1(?DS_SKILL_ID2(1, 2)), user = Unit1#ds_u.id},
        BaseDeadEvent = ds_event:new(0, 1),
        DeadEvent = BaseDeadEvent#ds_event{event = ?DS_E_NORMAL_DEAD, user = Unit1#ds_u.id},
        #ds{
            frame = 0,
            stream_event = #{1 => [Skill2Event], 2 => [Skill1Event]},
            trigger_event = #{?DS_ETTA_DEAD => [DeadEvent]},
            unit_map = #{
                Unit1#ds_u.id => Unit1,
                Unit2#ds_u.id => Unit2
            }
        }
                         end).


-ifdef(TEST).

base_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = ds_svr:start_link([]),
            sys:replace_state(Pid, fun(_) ->
                BaseEvent1 = ds_event:new(1, 1),
                Event1 = BaseEvent1#ds_event{user = 1, event = ?DS_E_TEST},
                BaseEvent2 = ds_event:new(3, 1),
                Event2 = BaseEvent2#ds_event{user = 2, event = ?DS_E_TEST},
                #ds{
                    stream_event = #{
                        1 => [Event1],
                        %% 这个事件不会执行
                        3 => [Event2]
                    },
                    unit_map = #{1 => #ds_u{id = 1, module = ds_test}, 2 => #ds_u{id = 2, module = ds_test}}
                }
                                   end),
            Pid
        end,
        fun(_) -> ok end,
        fun(Pid) ->
            ?_test(begin
                       Pid ! ?MSG_DS_NEXT_FRAME,
                       Pid ! ?MSG_DS_NEXT_FRAME
                   end)
        end
    }.

-endif.