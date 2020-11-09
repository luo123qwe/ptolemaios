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

-type unit_id() :: any().

-callback init(#dynames_unit{}, #dynames{}) -> {ok, #dynames_unit{}, #dynames_event{}}.
-callback filter_event_target(#dynames_unit{}, #dynames_event{}, #dynames{}) -> {ok, [unit_id()], #dynames_event{}}.
-callback execute_event(#dynames_unit{}, #dynames_unit{}, #dynames_event{}, #dynames{}) -> {ok, #dynames{}}.

%% @doc 造成伤害
-spec hurt(number(), #dynames_unit{}, #dynames_unit{}, #dynames_event{}, #dynames{}) -> #dynames{}.
hurt(Hurt, Target, Unit, Event, Dynames) ->
    #dynames{frame = Frame} = Dynames,
    #dynames_unit{id = TargetId, attr = TargetAttr} = Target,
    TargetHp = kv_op:lookup(?ATTR_HP, TargetAttr, 0),
    TargetHp1 = max(0, trunc(TargetHp - Hurt)),
    Dynames1 = kv_op:store([#dynames.unit_map, TargetId, #dynames_unit.attr, ?ATTR_HP], TargetHp1, Dynames),
    case TargetHp1 of
        0 ->% 死亡, 触发死亡事件
            SteamData = ?DYNAMES_DATA_DEAD(),
            dynames_event:trigger(dead, SteamData, Dynames1);
        _ ->
            Dynames1
    end.