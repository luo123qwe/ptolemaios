%%%-------------------------------------------------------------------
%% @doc ptolemaios top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ptolemaios_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
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
    SupFlags = #{strategy => one_for_one,
        intensity => 1,
        period => 5},

    %% 初始化一些不需要挂进程的东西
    local_lock:init_ets(),
    virture:init_ets(),

    %% 子进程
    {ok, User} = application:get_env(ptolemaios, mysql_user),
    {ok, Password} = application:get_env(ptolemaios, mysql_password),
    {ok, Database} = application:get_env(ptolemaios, mysql_database),
    PoolOptions = [{size, 50}, {max_overflow, 100}],
    MySqlOptions = [{user, User}, {password, Password}, {database, Database},
        {keep_alive, true},
        {prepare, [{test, "SELECT * FROM player WHERE id=?"}]}],
    ChildSpecs = [mysql_poolboy:child_spec(virture, PoolOptions, MySqlOptions)],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
