%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(client_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = #{id => client,
        start => {client_server, start_link, []},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => [client_server]},
    
    {ok, {#{strategy => simple_one_for_one,
        intensity => 5,
        period => 30},
        [AChild]}
    }.
