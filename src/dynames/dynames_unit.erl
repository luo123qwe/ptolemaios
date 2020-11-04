%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 战斗单位behaviour
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dynames_unit).
-author("dominic").

-include("dynames.hrl").

%% API
-export([]).

-type unit_id() :: any().

-callback filter_event_target(#dynames_unit{}, #dynames_event{}, #dynames{}) -> {ok, [unit_id()], #dynames_event{}}.
-callback execute_event([unit_id()], #dynames_unit{}, #dynames_event{}, #dynames{}) -> {ok, #dynames{}}.
