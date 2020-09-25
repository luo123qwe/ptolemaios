%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 玩家进程supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(player_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/1]).

%% @private
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
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

%% @doc 开启一个玩家进程
-spec start_child(non_neg_integer()) -> supervisor:startchild_ret().
start_child(PlayerId) ->
    supervisor:start_child(player_sup, [PlayerId]).