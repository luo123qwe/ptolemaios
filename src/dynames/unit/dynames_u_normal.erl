%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 普通怪物
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dynames_u_normal).
-author("dominic").

-behaviour(dynames_unit).

-include("util.hrl").
-include("dynames.hrl").
-include("attr.hrl").

-define(ID_1, 1).% 近战
-define(ID_2, 2).% 远程

%% API
-export([init/2, filter_event_target/3, execute_event/4]).

init(Unit, Dynames) ->
    ?LOG_NOTICE("~p", [Unit#dynames_unit.data_id]),
    {ok, Unit, Dynames}.

%% 近战/远程爆发
filter_event_target(Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_1, 1);SkillId == ?DYNAMES_SKILL_ID2(?ID_2, 1) ->
    %% 除自己以外的都是目标
    OtherUnitMap = dynames:other_unit(Unit, Dynames#dynames.unit_map),
    #data_dynames_unit{radius = Radius} = data_dynames_unit:get(Unit#dynames_unit.data_id),
    RadiusUnitMap = dynames:unit_in_radius(Radius, Unit, OtherUnitMap),
    TargetIdList = maps:keys(RadiusUnitMap),
    {ok, TargetIdList, Event};
%% 近战加攻击
filter_event_target(Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)} = Event, _Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_1, 1) ->
    %% 自己加
    {ok, [Unit#dynames_unit.id], Event};
%% 远程减攻击
filter_event_target(Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_2, 1) ->
    %% 别人减
    OtherUnitMap = dynames:other_unit(Unit, Dynames#dynames.unit_map),
    #data_dynames_unit{radius = Radius} = data_dynames_unit:get(Unit#dynames_unit.data_id),
    RadiusUnitMap = dynames:unit_in_radius(Radius, Unit, OtherUnitMap),
    TargetIdList = maps:keys(RadiusUnitMap),
    {ok, TargetIdList, Event};
filter_event_target(_Unit, Event, _Dynames) ->
    ?LOG_ERROR("unknown event: ~w", [Event]),
    {ok, [], Event}.

%% 近战/远程攻击
execute_event(Target, Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_1, 1);SkillId == ?DYNAMES_SKILL_ID2(?ID_2, 1) ->
    #dynames_unit{state = TargetState, attr = TargetAttr} = Target,
    ?DYNAMES_RETURN_IF1(TargetState =/= ?DYNAMES_UNIT_STATE_ALIVE),
    TargetHp = kv_op:lookup(?ATTR_HP, TargetAttr, 0),
    ?DYNAMES_RETURN_IF1(TargetHp =< 0),
    #data_dynames_unit{skill_arg_1 = SkillArg1} = data_dynames_unit:get(Unit#dynames_unit.data_id),
    Rate = kv_op:lookup(rate, SkillArg1, 0),
    ?DYNAMES_RETURN_IF1(Rate == 0),
    
    %% 计算伤害
    #dynames_unit{attr = UnitAttr} = Unit,
    TargetDefense = kv_op:lookup(?ATTR_DEFENSE, TargetAttr, 0),
    UnitAttack = kv_op:lookup(?ATTR_ATTACK, UnitAttr, 0),
    Hurt = UnitAttack - TargetDefense,
    
    %% 扣血
    Dynames1 = dynames_unit:hurt(Hurt, Target, Unit, Event, Dynames),
    {ok, Dynames1};
%% 近战加攻击
execute_event(Target, Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_1, 1) ->
    
    
    {ok, Dynames1};
execute_event(_Target, _Unit, Event, Dynames) ->
    ?LOG_ERROR("unknown event: ~w", [Event]),
    {ok, Dynames}.