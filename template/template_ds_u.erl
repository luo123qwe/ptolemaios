%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 战斗单位
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(template_ds_u).
-author("dominic").

-behaviour(ds_unit).

%% API
-export([init/2, filter_event_target/3, execute_event/4]).

init(Unit, Dynames) ->
    {ok, Unit, Dynames}.

filter_event_target(_Unit, Event, _Dynames) ->
    {ok, [], Event}.

execute_event(_Target, _Unit, _Event, Dynames) ->
    {ok, Dynames}.