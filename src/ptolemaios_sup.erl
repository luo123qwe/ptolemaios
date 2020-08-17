%%%-------------------------------------------------------------------
%% @doc ptolemaios top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ptolemaios_sup).

-behaviour(supervisor).

-include("util.hrl").

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    log:init(),% 日志
    
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    
    local_lock:init_ets(),
    
    %% 子进程
    {ok, Port} = application:get_env(ptolemaios, port),
    ListenerSpec = ranch:child_spec(gateway, ranch_tcp, #{socket_opts => [{port, Port}]}, gateway, []),
    VirtureSpec = virture_config:get_sup_spec(mysql),
    ChildSpecs = [ListenerSpec | VirtureSpec],
    
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
