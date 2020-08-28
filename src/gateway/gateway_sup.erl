%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(gateway_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = application:get_env(gateway, port),
    ListenerSpec = ranch:child_spec(gateway, ranch_tcp, #{socket_opts => [{port, Port}]}, gateway_server, []),
    
    {ok, {#{strategy => one_for_one,
        intensity => 5,
        period => 30},
        [#{id => client_sup, start => {client_sup, start_link, []}, type => supervisor}, ListenerSpec]}
    }.
