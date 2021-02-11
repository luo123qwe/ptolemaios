%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(battle_mapping).
-author("dominic").

%% API
-export([actor_module/1]).

%% @doc 获取配置表单位id对应的回调模块, 这里修改频率极低, 直接硬编码
actor_module(1) ->
    battle_u_normal;
actor_module(2) ->
    battle_u_normal;
actor_module(_Id) ->
    undefined.