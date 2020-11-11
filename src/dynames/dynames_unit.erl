%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 战斗单位behaviour
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dynames_unit).
-author("dominic").

-include("util.hrl").
-include("dynames.hrl").
-include("attr.hrl").

%% API

-export([hurt/5]).

-callback init(#dynames_unit{}, #dynames{}) -> {ok, #dynames_unit{}, #dynames_event{}}.
-callback filter_event_target(#dynames_unit{}, #dynames_event{}, #dynames{}) -> {ok, #{Id :: any() => #dynames_unit{}}, #dynames_event{}}.
%% 处理事件传入的unit是旧数据, 需要重新读取再执行
-callback execute_event(#dynames_unit{}, #dynames_unit{}, #dynames_event{}, #dynames{}) -> {ok, #dynames{}}.

%% @doc 造成伤害
-spec hurt(number(), #dynames_unit{}, #dynames_unit{}, #dynames_event{}, #dynames{}) -> #dynames{}.
hurt(Hurt, Target, Unit, Event, Dynames) ->
    #dynames_unit{id = TargetId, attr = TargetAttr} = Target,
    TargetHp = kv_op:lookup(?ATTR_HP, TargetAttr, 0),
    TargetHp1 = max(0, trunc(TargetHp - Hurt)),
    Dynames1 = kv_op:store([#dynames.unit_map, TargetId, #dynames_unit.attr, ?ATTR_HP], TargetHp1, Dynames),
    case TargetHp1 of
        0 ->% 死亡, 触发死亡事件
            SteamData = #dynames_esd_dead{
                killer = Unit#dynames_unit.id,
                dead = TargetId, event = Event
            },
            dead(SteamData, Dynames1);
        _ ->
            Dynames1
    end.

dead(SteamData, Dynames) ->
    Dynames1 = dynames_event:trigger(?DYNAMES_ETTB_DEAD, SteamData, Dynames),
    case kv_op:update(fun(_, Unit) ->
        case kv_op:lookup(?ATTR_HP, Unit#dynames_unit.attr, 0) > 0 of
            true ->
                {false, true, Unit};
            _ ->
                {false, false, Unit#dynames_unit{state = ?DYNAMES_UNIT_STATE_DEAD}}
        end
                      end, [#dynames.unit_map, SteamData#dynames_esd_dead.dead], Dynames1)
    of
        undefined ->% 移出出游戏了
            dynames_event:trigger(?DYNAMES_ETTA_DEAD, SteamData, Dynames1);
        {true, Dynames2} ->% 又活了
            Dynames2;
        {_, Dynames2} ->
            dynames_event:trigger(?DYNAMES_ETTA_DEAD, SteamData, Dynames2)
    end.
    
    
    
    
    