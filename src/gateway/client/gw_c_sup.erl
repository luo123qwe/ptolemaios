%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 客户端进程supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(gw_c_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/0, start_child/4]).

%% @private
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
init([]) ->
    AChild = #{id => client,
        start => {gw_c_svr, start_link, []},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => [gw_c_svr]},
    
    {ok, {#{strategy => simple_one_for_one,
        intensity => 5,
        period => 30},
        [AChild]}
    }.

%% @doc 开启一个新的客户端进程, 使用配置的port和gw_c_echo
%% @equiv start_child({127,0,0,1}, ConfigPort, gw_c_echo, [])
-spec start_child() -> supervisor:startchild_ret().
start_child() ->
    {ok, Config} = application:get_env(ptolemaios, gateway),
    Port = proplists:get_value(port, Config),
    start_child({127, 0, 0, 1}, Port, gw_c_echo, []).

%% @doc 开启一个新的客户端进程, Module为回调函数, 实现gw_c_svr behaviour, Args为init回调的参数
-spec start_child(inet:socket_address() | inet:hostname(), inet:port_number(), atom(), term()) -> supervisor:startchild_ret().
start_child(Ip, Port, Module, Args) ->
    supervisor:start_child(gw_c_sup, [Ip, Port, Module, Args]).
