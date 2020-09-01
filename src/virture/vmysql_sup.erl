%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(vmysql_sup).

-behaviour(supervisor).

-include("util.hrl").
-include("virture.hrl").

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% 这里可能有修复
    vmysql:system_init(),
    
    {ok, {#{strategy => one_for_one,
        intensity => 1,
        period => 5},
        []}
    }.