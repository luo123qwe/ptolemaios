%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 普通怪物
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ds_u_normal).
-author("dominic").

-behaviour(ds_unit).

-include("ptolemaios_lib.hrl").
-include("ds.hrl").
-include("attr.hrl").

-define(ID_1, 1).% 近战
-define(ID_2, 2).% 远程

%% API
-export([init/2, filter_event_target/3, execute_event/4]).

init(Unit, Dynames) ->
    {ok, Unit, Dynames}.

%% 近战/远程普攻
filter_event_target(Unit, #ds_event{event = ?DS_E_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DS_SKILL_ID2(?ID_1, 0);SkillId == ?DS_SKILL_ID2(?ID_2, 0) ->
    #ds_u{data_id = UnitDataId} = Unit,
    #ds{unit_map = UnitMap} = Dynames,
    #data_ds_u{radius = Radius} = data_ds_u:get(UnitDataId),
    TargetMap = filter_other_radius(Radius, Unit, UnitMap),
    {ok, TargetMap, Event};
%% 近战/远程爆发
filter_event_target(Unit, #ds_event{event = ?DS_E_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DS_SKILL_ID2(?ID_1, 1);SkillId == ?DS_SKILL_ID2(?ID_2, 1) ->
    #ds_u{data_id = UnitDataId} = Unit,
    #ds{unit_map = UnitMap} = Dynames,
    Radius = ptolemaios_kv:lookup([#data_ds_u.skill_arg_1, radius], data_ds_u:get(UnitDataId), undefined),
    TargetMap = filter_other_radius(Radius, Unit, UnitMap),
    {ok, TargetMap, Event};
%% 近战加攻击
filter_event_target(Unit, #ds_event{event = ?DS_E_SKILL1(SkillId)} = Event, _Dynames)
    when SkillId == ?DS_SKILL_ID2(?ID_1, 2) ->
    %% 自己加
    {ok, #{Unit#ds_u.id => Unit}, Event};
%% 远程减攻击
filter_event_target(Unit, #ds_event{event = ?DS_E_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DS_SKILL_ID2(?ID_2, 2) ->
    #ds_u{data_id = UnitDataId} = Unit,
    #ds{unit_map = UnitMap} = Dynames,
    Radius = ptolemaios_kv:lookup([#data_ds_u.skill_arg_1, radius], data_ds_u:get(UnitDataId), undefined),
    TargetMap = filter_other_radius(Radius, Unit, UnitMap),
    {ok, TargetMap, Event};
%% 死亡
filter_event_target(Unit, #ds_event{event = ?DS_E_NORMAL_DEAD} = Event, _Dynames) ->
    {ok, #{Unit#ds_u.id => Unit}, Event};
filter_event_target(_Unit, Event, _Dynames) ->
    ?LOG_ERROR("unknown event: ~w", [Event]),
    {ok, [], Event}.

%% 近战/远程攻击
execute_event(Target, Unit, #ds_event{event = ?DS_E_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DS_SKILL_ID2(?ID_1, 1);SkillId == ?DS_SKILL_ID2(?ID_2, 1) ->
    #ds_u{state = TargetState, attr = TargetAttr} = Target,
    ?DS_R_IF1(TargetState =/= ?DS_UNIT_STATE_ALIVE),
    #data_ds_u{skill_arg_1 = SkillArg1} = data_ds_u:get(Unit#ds_u.data_id),
    Rate = ptolemaios_kv:lookup(rate, SkillArg1, 0),
    
    %% 计算伤害
    #ds_u{attr = UnitAttr} = Unit,
    TargetDefense = ptolemaios_kv:lookup(?ATTR_DEFENSE, TargetAttr, 0),
    UnitAttack = ptolemaios_kv:lookup(?ATTR_ATTACK, UnitAttr, 0),
    Hurt = (UnitAttack - TargetDefense) * Rate,
    
    %% 扣血
    Dynames1 = ds_unit:hurt(Hurt, Target, Unit, Event, Dynames),
    ?LOG_NOTICE("~w ~w", [Target#ds_u.id, Event]),
    {ok, Dynames1};
%% 近战加攻击
execute_event(Target, Unit, #ds_event{event = ?DS_E_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DS_SKILL_ID2(?ID_1, 2) ->
    #ds_u{data_id = UnitDataId} = Unit,
    #ds_u{id = TargetId, state = TargetState, attr = TargetAttr} = Target,
    ?DS_R_IF1(TargetState =/= ?DS_UNIT_STATE_ALIVE),
    
    %% 加属性
    Add = ptolemaios_kv:lookup([#data_ds_u.skill_arg_2, attack], data_ds_u:get(UnitDataId), 0),
    TargetAttr1 = ptolemaios_kv:plus([TargetAttr, [{?ATTR_ATTACK, Add}]]),
    Target1 = Target#ds_u{attr = TargetAttr1},
    
    Dynames1 = ptolemaios_kv:store([#ds.unit_map, TargetId], Target1, Dynames),
    ?LOG_NOTICE("~w ~w", [Target#ds_u.id, Event]),
    {ok, Dynames1};
%% 远程减攻击
execute_event(Target, Unit, #ds_event{event = ?DS_E_SKILL1(SkillId)} = Event, Dynames)
    when SkillId == ?DS_SKILL_ID2(?ID_2, 2) ->
    #ds_u{data_id = UnitDataId} = Unit,
    #ds_u{id = TargetId, state = TargetState, attr = TargetAttr} = Target,
    ?DS_R_IF1(TargetState =/= ?DS_UNIT_STATE_ALIVE),
    
    %% 减属性
    Add = ptolemaios_kv:lookup([#data_ds_u.skill_arg_2, attack], data_ds_u:get(UnitDataId), 0),
    TargetAttr1 = ptolemaios_kv:subtract([TargetAttr, [{?ATTR_ATTACK, Add}]]),
    Target1 = Target#ds_u{attr = TargetAttr1},
    
    Dynames1 = ptolemaios_kv:store([#ds.unit_map, TargetId], Target1, Dynames),
    ?LOG_NOTICE("~w ~w", [Target#ds_u.id, Event]),
    {ok, Dynames1};
%% 死亡回调
execute_event(Target, _Unit, #ds_event{event = ?DS_E_NORMAL_DEAD} = Event, Dynames) ->
    %% todo 仅打印一句
    ?LOG_NOTICE("~w ~w", [Target#ds_u.id, Event]),
    {ok, Dynames};
execute_event(_Target, _Unit, Event, Dynames) ->
    ?LOG_ERROR("unknown event: ~w", [Event]),
    {ok, Dynames}.


filter_other_radius(Radius, #ds_u{id = UnitId, x = UnitX, y = UnitY}, UnitMap) ->
    maps:filter(fun(K, V) ->
        %% 除自己以外, 在范围内的
        K =/= UnitId andalso ds:distance(UnitX, UnitY, V) =< Radius
                end, UnitMap).

