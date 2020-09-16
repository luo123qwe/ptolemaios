%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fix_restart_1).
-author("dominic").

-behaviour(fix_restart).

-include("util.hrl").

%% API
-export([run/0]).

run() ->
    ?LOG_NOTICE("this is an example").
