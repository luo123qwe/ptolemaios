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
    {ok, Unit, Dynames}.

%% 近战/远程普攻
filter_event_target(Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_1, 0);SkillId == ?DYNAMES_SKILL_ID2(?ID_2, 0) ->
    #dynames_unit{data_id = UnitDataId} = Unit,
    #dynames{unit_map = UnitMap} = Dynames,
    #data_dynames_unit{radius = Radius} = data_dynames_unit:get(UnitDataId),
    TargetMap = filter_other_radius(Radius, Unit, UnitMap),
    {ok, TargetMap, Event};
%% 近战/远程爆发
filter_event_target(Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_1, 1);SkillId == ?DYNAMES_SKILL_ID2(?ID_2, 1) ->
    #dynames_unit{data_id = UnitDataId} = Unit,
    #dynames{unit_map = UnitMap} = Dynames,
    Radius = kv_op:lookup([#data_dynames_unit.skill_arg_1, radius], data_dynames_unit:get(UnitDataId), undefined),
    TargetMap = filter_other_radius(Radius, Unit, UnitMap),
    {ok, TargetMap, Event};
%% 近战加攻击
filter_event_target(Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)} = Event, _Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_1, 2) ->
    %% 自己加
    {ok, #{Unit#dynames_unit.id => Unit}, Event};
%% 远程减攻击
filter_event_target(Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_2, 2) ->
    #dynames_unit{data_id = UnitDataId} = Unit,
    #dynames{unit_map = UnitMap} = Dynames,
    Radius = kv_op:lookup([#data_dynames_unit.skill_arg_1, radius], data_dynames_unit:get(UnitDataId), undefined),
    TargetMap = filter_other_radius(Radius, Unit, UnitMap),
    {ok, TargetMap, Event};
filter_event_target(_Unit, Event, _Dynames) ->
    ?LOG_ERROR("unknown event: ~w", [Event]),
    {ok, [], Event}.

%% 近战/远程攻击
execute_event(Target, Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_1, 1);SkillId == ?DYNAMES_SKILL_ID2(?ID_2, 1) ->
    #dynames_unit{state = TargetState, attr = TargetAttr} = Target,
    ?DYNAMES_RETURN_IF1(TargetState =/= ?DYNAMES_UNIT_STATE_ALIVE),
    #data_dynames_unit{skill_arg_1 = SkillArg1} = data_dynames_unit:get(Unit#dynames_unit.data_id),
    Rate = kv_op:lookup(rate, SkillArg1, 0),
    
    %% 计算伤害
    #dynames_unit{attr = UnitAttr} = Unit,
    TargetDefense = kv_op:lookup(?ATTR_DEFENSE, TargetAttr, 0),
    UnitAttack = kv_op:lookup(?ATTR_ATTACK, UnitAttr, 0),
    Hurt = (UnitAttack - TargetDefense) * Rate,
    
    %% 扣血
    Dynames1 = dynames_unit:hurt(Hurt, Target, Unit, Event, Dynames),
    {ok, Dynames1};
%% 近战加攻击
execute_event(Target, Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)}, Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_1, 2) ->
    #dynames_unit{data_id = UnitDataId} = Unit,
    #dynames_unit{id = TargetId, state = TargetState, attr = TargetAttr} = Target,
    ?DYNAMES_RETURN_IF1(TargetState =/= ?DYNAMES_UNIT_STATE_ALIVE),
    
    %% 加属性
    Add = kv_op:lookup([#data_dynames_unit.skill_arg_2, attack], data_dynames_unit:get(UnitDataId), 0),
    TargetAttr1 = kv_op:plus([TargetAttr, [{?ATTR_ATTACK, Add}]]),
    Target1 = Target#dynames_unit{attr = TargetAttr1},
    
    Dynames1 = kv_op:store([#dynames.unit_map, TargetId], Target1, Dynames),
    {ok, Dynames1};
%% 远程减攻击
execute_event(Target, Unit, #dynames_event{event = ?DYNAMES_EVENT_SKILL1(SkillId)}, Dynames)
    when SkillId == ?DYNAMES_SKILL_ID2(?ID_2, 2) ->
    #dynames_unit{data_id = UnitDataId} = Unit,
    #dynames_unit{id = TargetId, state = TargetState, attr = TargetAttr} = Target,
    ?DYNAMES_RETURN_IF1(TargetState =/= ?DYNAMES_UNIT_STATE_ALIVE),
    
    %% 减属性
    Add = kv_op:lookup([#data_dynames_unit.skill_arg_2, attack], data_dynames_unit:get(UnitDataId), 0),
    TargetAttr1 = kv_op:subtract([TargetAttr, [{?ATTR_ATTACK, Add}]]),
    Target1 = Target#dynames_unit{attr = TargetAttr1},
    
    Dynames1 = kv_op:store([#dynames.unit_map, TargetId], Target1, Dynames),
    {ok, Dynames1};
execute_event(_Target, _Unit, Event, Dynames) ->
    ?LOG_ERROR("unknown event: ~w", [Event]),
    {ok, Dynames}.


filter_other_radius(Radius, #dynames_unit{id = UnitId, x = UnitX, y = UnitY}, UnitMap) ->
    maps:filter(fun(K, V) ->
        %% 除自己以外, 在范围内的
        K =/= UnitId andalso dynames:distance(UnitX, UnitY, V) =< Radius
                end, UnitMap).

