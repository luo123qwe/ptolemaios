%%%-------------------------------------------------------------------
%% @doc ptolemaios top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ptolemaios_sup).

-behaviour(supervisor).

-include("util.hrl").
-include("virture.hrl").

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
    
    proto_mapping:load(),
    
    %% 子进程
    {ok, Port} = application:get_env(ptolemaios, port),
    ListenerSpec = ranch:child_spec(gateway, ranch_tcp, #{socket_opts => [{port, Port}]}, gateway_server, []),
    MysqlSpec = make_mysql_specs(),
    VirtureSpec = ?IF(MysqlSpec == [], [], MysqlSpec ++ virture_config:get_sup_spec(mysql)),
    ChildSpecs = [ListenerSpec] ++ VirtureSpec,
    
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

make_mysql_specs() ->
    try
        {ok, User} = application:get_env(ptolemaios, mysql_user),
        {ok, Password} = application:get_env(ptolemaios, mysql_password),
        {ok, Database} = application:get_env(ptolemaios, mysql_database),
        PoolOptions = [{size, 50}, {max_overflow, 100}],
        MySqlOptions = [{user, User}, {password, Password}, {database, Database},
            {keep_alive, true},
            {prepare, []}],%{test, "SELECT * FROM player WHERE id=?"}]}],
        [mysql_poolboy:child_spec(?VMYSQL_POOL, PoolOptions, MySqlOptions)]
    catch
        C:E ->
            ?LOG_WARNING("bad mysql config, ~p", [{C, E}]),
            []
    end.