%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 网关sup
%%% @end
%%%-------------------------------------------------------------------
-module(gateway_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Config} = application:get_env(ptolemaios, gateway),
    Port = proplists:get_value(port, Config),
    ListenerSpec = ranch:child_spec(gateway, ranch_tcp, #{socket_opts => [{port, Port}]}, gateway_svr, []),
    
    {ok, {#{strategy => one_for_one,
        intensity => 5,
        period => 30},
        [#{id => gateway_c_sup, start => {gateway_c_sup, start_link, []}, type => supervisor}, ListenerSpec]}
    }.
