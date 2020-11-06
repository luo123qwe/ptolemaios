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
        start => {player_svr, start_link, []},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => [player_svr]},
    
    {ok, {#{strategy => simple_one_for_one,
        intensity => 5,
        period => 30},
        [AChild]}
    }.

%% @doc 开启一个玩家进程
-spec start_child(list()) -> supervisor:startchild_ret().
start_child(Args) ->
    supervisor:start_child(player_sup, [Args]).