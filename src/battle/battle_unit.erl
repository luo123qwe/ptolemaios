%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 战斗单位behaviour
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(battle_unit).
-author("dominic").

-include("plm_lib.hrl").
-include("battle.hrl").
-include("attr.hrl").

%% API

-export([new/1, hurt/5]).

-callback init(#battle_unit{}, #battle{}) -> {ok, #battle_unit{}, #battle_event{}}.
-callback filter_event_target(#battle_unit{}, #battle_event{}, #battle{}) -> {ok, #{Id :: any() => #battle_unit{}}, #battle_event{}}.
%% 处理事件传入的unit是旧数据, 需要重新读取再执行
-callback execute_event(#battle_unit{}, #battle_unit{}, #battle_event{}, #battle{}) -> {ok, #battle{}}.

%% todo 根据具体需求修改
new(DataId) ->
    #battle_unit{
        id = battle:get_id(?M14_ID_TYPE_UNIT),
        data_id = DataId,
        module = battle_mapping:actor_module(DataId)
    }.

%% @doc 造成伤害
-spec hurt(number(), #battle_unit{}, #battle_unit{}, #battle_event{}, #battle{}) -> #battle{}.
hurt(Hurt, Target, Unit, Event, Battle) ->
    #battle_unit{id = TargetId, attr = TargetAttr} = Target,
    TargetHp = plm_kv:lookup(?ATTR_HP, TargetAttr, 0),
    TargetHp1 = max(0, trunc(TargetHp - Hurt)),
    Battle1 = plm_kv:store([#battle.unit_map, TargetId, #battle_unit.attr, ?ATTR_HP], TargetHp1, Battle),
    case TargetHp1 of
        0 ->% 死亡, 触发死亡事件
            SteamData = #battle_esd_dead{
                killer = Unit#battle_unit.id,
                dead = TargetId, event = Event
            },
            dead(SteamData, Battle1);
        _ ->
            Battle1
    end.

dead(SteamData, Battle) ->
    Battle1 = battle_event:trigger(?M14_ETTB_DEAD, SteamData, Battle),
    case plm_kv:update(fun(_, Unit) ->
        case plm_kv:lookup(?ATTR_HP, Unit#battle_unit.attr, 0) > 0 of
            true ->
                {false, true, Unit};
            _ ->
                {false, false, Unit#battle_unit{state = ?M14_UNIT_STATE_DEAD}}
        end
                      end, [#battle.unit_map, SteamData#battle_esd_dead.dead], Battle1)
    of
        undefined ->% 移出游戏了
            battle_event:trigger(?M14_ETTA_DEAD, SteamData, Battle1);
        {true, Battle2} ->% 又活了
            Battle2;
        {_, Battle2} ->
            battle_event:trigger(?M14_ETTA_DEAD, SteamData, Battle2)
    end.
    
    
    
    
    