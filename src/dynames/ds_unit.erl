%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 战斗单位behaviour
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ds_unit).
-author("dominic").

-include("ptolemaios_lib.hrl").
-include("ds.hrl").
-include("attr.hrl").

%% API

-export([new/1, hurt/5]).

-callback init(#ds_u{}, #ds{}) -> {ok, #ds_u{}, #ds_event{}}.
-callback filter_event_target(#ds_u{}, #ds_event{}, #ds{}) -> {ok, #{Id :: any() => #ds_u{}}, #ds_event{}}.
%% 处理事件传入的unit是旧数据, 需要重新读取再执行
-callback execute_event(#ds_u{}, #ds_u{}, #ds_event{}, #ds{}) -> {ok, #ds{}}.

%% todo 根据具体需求修改
new(DataId) ->
    #ds_u{
        id = ds:get_id(?DS_ID_TYPE_UNIT),
        data_id = DataId,
        module = ds_mapping:actor_module(DataId)
    }.

%% @doc 造成伤害
-spec hurt(number(), #ds_u{}, #ds_u{}, #ds_event{}, #ds{}) -> #ds{}.
hurt(Hurt, Target, Unit, Event, Dynames) ->
    #ds_u{id = TargetId, attr = TargetAttr} = Target,
    TargetHp = plm_kv:lookup(?ATTR_HP, TargetAttr, 0),
    TargetHp1 = max(0, trunc(TargetHp - Hurt)),
    Dynames1 = plm_kv:store([#ds.unit_map, TargetId, #ds_u.attr, ?ATTR_HP], TargetHp1, Dynames),
    case TargetHp1 of
        0 ->% 死亡, 触发死亡事件
            SteamData = #ds_esd_dead{
                killer = Unit#ds_u.id,
                dead = TargetId, event = Event
            },
            dead(SteamData, Dynames1);
        _ ->
            Dynames1
    end.

dead(SteamData, Dynames) ->
    Dynames1 = ds_event:trigger(?DS_ETTB_DEAD, SteamData, Dynames),
    case plm_kv:update(fun(_, Unit) ->
        case plm_kv:lookup(?ATTR_HP, Unit#ds_u.attr, 0) > 0 of
            true ->
                {false, true, Unit};
            _ ->
                {false, false, Unit#ds_u{state = ?DS_UNIT_STATE_DEAD}}
        end
                      end, [#ds.unit_map, SteamData#ds_esd_dead.dead], Dynames1)
    of
        undefined ->% 移出游戏了
            ds_event:trigger(?DS_ETTA_DEAD, SteamData, Dynames1);
        {true, Dynames2} ->% 又活了
            Dynames2;
        {_, Dynames2} ->
            ds_event:trigger(?DS_ETTA_DEAD, SteamData, Dynames2)
    end.
    
    
    
    
    