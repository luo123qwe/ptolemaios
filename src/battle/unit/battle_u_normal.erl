%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 普通怪物, u = unit
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(battle_u_normal).
-author("dominic").

-behaviour(battle_unit).

-include("plm_lib.hrl").
-include("battle.hrl").
-include("attr.hrl").

-define(ID_1, 1).% 近战
-define(ID_2, 2).% 远程

%% API
-export([init/2, filter_event_target/3, execute_event/4]).

init(Unit, Battle) ->
    {ok, Unit, Battle}.

%% 近战/远程普攻
filter_event_target(Unit, #battle_event{event = ?M14_E_SKILL1(SkillId)} = Event, Battle)
    when SkillId == ?M14_SKILL_ID2(?ID_1, 0);SkillId == ?M14_SKILL_ID2(?ID_2, 0) ->
    #battle_unit{data_id = UnitDataId} = Unit,
    #battle{unit_map = UnitMap} = Battle,
    #data_battle_u{radius = Radius} = data_battle_u:get(UnitDataId),
    TargetMap = filter_other_radius(Radius, Unit, UnitMap),
    {ok, TargetMap, Event};
%% 近战/远程爆发
filter_event_target(Unit, #battle_event{event = ?M14_E_SKILL1(SkillId)} = Event, Battle)
    when SkillId == ?M14_SKILL_ID2(?ID_1, 1);SkillId == ?M14_SKILL_ID2(?ID_2, 1) ->
    #battle_unit{data_id = UnitDataId} = Unit,
    #battle{unit_map = UnitMap} = Battle,
    Radius = plm_kv:lookup([#data_battle_u.skill_arg_1, radius], data_battle_u:get(UnitDataId), undefined),
    TargetMap = filter_other_radius(Radius, Unit, UnitMap),
    {ok, TargetMap, Event};
%% 近战加攻击
filter_event_target(Unit, #battle_event{event = ?M14_E_SKILL1(SkillId)} = Event, _Battle)
    when SkillId == ?M14_SKILL_ID2(?ID_1, 2) ->
    %% 自己加
    {ok, #{Unit#battle_unit.id => Unit}, Event};
%% 远程减攻击
filter_event_target(Unit, #battle_event{event = ?M14_E_SKILL1(SkillId)} = Event, Battle)
    when SkillId == ?M14_SKILL_ID2(?ID_2, 2) ->
    #battle_unit{data_id = UnitDataId} = Unit,
    #battle{unit_map = UnitMap} = Battle,
    Radius = plm_kv:lookup([#data_battle_u.skill_arg_1, radius], data_battle_u:get(UnitDataId), undefined),
    TargetMap = filter_other_radius(Radius, Unit, UnitMap),
    {ok, TargetMap, Event};
%% 死亡
filter_event_target(Unit, #battle_event{event = ?M14_E_NORMAL_DEAD} = Event, _Battle) ->
    {ok, #{Unit#battle_unit.id => Unit}, Event};
filter_event_target(_Unit, Event, _Battle) ->
    ?LOG_ERROR("unknown event: ~w", [Event]),
    {ok, [], Event}.

%% 近战/远程攻击
execute_event(Target, Unit, #battle_event{event = ?M14_E_SKILL1(SkillId)} = Event, Battle)
    when SkillId == ?M14_SKILL_ID2(?ID_1, 1);SkillId == ?M14_SKILL_ID2(?ID_2, 1) ->
    #battle_unit{state = TargetState, attr = TargetAttr} = Target,
    ?M14_IF1(TargetState =/= ?M14_UNIT_STATE_ALIVE),
    #data_battle_u{skill_arg_1 = SkillArg1} = data_battle_u:get(Unit#battle_unit.data_id),
    Rate = plm_kv:lookup(rate, SkillArg1, 0),
    
    %% 计算伤害
    #battle_unit{attr = UnitAttr} = Unit,
    TargetDefense = plm_kv:lookup(?ATTR_DEFENSE, TargetAttr, 0),
    UnitAttack = plm_kv:lookup(?ATTR_ATTACK, UnitAttr, 0),
    Hurt = (UnitAttack - TargetDefense) * Rate,
    
    %% 扣血
    Battle1 = battle_unit:hurt(Hurt, Target, Unit, Event, Battle),
    ?LOG_NOTICE("~w ~w", [Target#battle_unit.id, Event]),
    {ok, Battle1};
%% 近战加攻击
execute_event(Target, Unit, #battle_event{event = ?M14_E_SKILL1(SkillId)} = Event, Battle)
    when SkillId == ?M14_SKILL_ID2(?ID_1, 2) ->
    #battle_unit{data_id = UnitDataId} = Unit,
    #battle_unit{id = TargetId, state = TargetState, attr = TargetAttr} = Target,
    ?M14_IF1(TargetState =/= ?M14_UNIT_STATE_ALIVE),
    
    %% 加属性
    Add = plm_kv:lookup([#data_battle_u.skill_arg_2, attack], data_battle_u:get(UnitDataId), 0),
    TargetAttr1 = plm_kv:plus([TargetAttr, [{?ATTR_ATTACK, Add}]]),
    Target1 = Target#battle_unit{attr = TargetAttr1},
    
    Battle1 = plm_kv:store([#battle.unit_map, TargetId], Target1, Battle),
    ?LOG_NOTICE("~w ~w", [Target#battle_unit.id, Event]),
    {ok, Battle1};
%% 远程减攻击
execute_event(Target, Unit, #battle_event{event = ?M14_E_SKILL1(SkillId)} = Event, Battle)
    when SkillId == ?M14_SKILL_ID2(?ID_2, 2) ->
    #battle_unit{data_id = UnitDataId} = Unit,
    #battle_unit{id = TargetId, state = TargetState, attr = TargetAttr} = Target,
    ?M14_IF1(TargetState =/= ?M14_UNIT_STATE_ALIVE),
    
    %% 减属性
    Add = plm_kv:lookup([#data_battle_u.skill_arg_2, attack], data_battle_u:get(UnitDataId), 0),
    TargetAttr1 = plm_kv:subtract([TargetAttr, [{?ATTR_ATTACK, Add}]]),
    Target1 = Target#battle_unit{attr = TargetAttr1},
    
    Battle1 = plm_kv:store([#battle.unit_map, TargetId], Target1, Battle),
    ?LOG_NOTICE("~w ~w", [Target#battle_unit.id, Event]),
    {ok, Battle1};
%% 死亡回调
execute_event(Target, _Unit, #battle_event{event = ?M14_E_NORMAL_DEAD} = Event, Battle) ->
    %% todo 仅打印一句
    ?LOG_NOTICE("~w ~w", [Target#battle_unit.id, Event]),
    {ok, Battle};
execute_event(_Target, _Unit, Event, Battle) ->
    ?LOG_ERROR("unknown event: ~w", [Event]),
    {ok, Battle}.


filter_other_radius(Radius, #battle_unit{id = UnitId, x = UnitX, y = UnitY}, UnitMap) ->
    maps:filter(fun(K, V) ->
        %% 除自己以外, 在范围内的
        K =/= UnitId andalso battle:distance(UnitX, UnitY, V) =< Radius
                end, UnitMap).

