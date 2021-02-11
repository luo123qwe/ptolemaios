%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 战斗单位
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(template_battle_u).
-author("dominic").

-behaviour(battle_unit).

%% API
-export([init/2, filter_event_target/3, execute_event/4]).

init(Unit, Battle) ->
    {ok, Unit, Battle}.

filter_event_target(_Unit, Event, _Battle) ->
    {ok, [], Event}.

execute_event(_Target, _Unit, _Event, Battle) ->
    {ok, Battle}.