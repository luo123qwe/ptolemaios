%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(player_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    AChild = #{id => player,
        start => {player_server, start_link, []},
        restart => transient,
        shutdown => 2000,
        type => worker,
        modules => [player_server]},
    
    {ok, {#{strategy => simple_one_for_one,
        intensity => 5,
        period => 30},
        [AChild]}
    }.

start_child(Id) ->
    supervisor:start_child(player_sup, [Id]).