%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fix_restart_1).
-author("dominic").

-behaviour(fix).

-include("util.hrl").

%% API
-export([fix/0, fix_again/0]).

fix() ->
    ?LOG_NOTICE("make an error"),
    error(?MODULE).

fix_again() ->
    ?LOG_NOTICE("this is an example").
