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
    %% 初始化循环日志, 自定义格式
    ok = logger:add_handler(ptolemaios, logger_disk_log_h, #{config => #{file => "./log/log",
        type => wrap,
        max_no_files => 10,
        max_no_bytes => 30000},
        filesync_repeat_interval => 5000}),
    logger:update_formatter_config(?MODULE, #{template => [level, " ", time, " ", pid, " ", mfa, ":", line, "\n", msg, "\n"]}),
    logger:update_formatter_config(default, #{template => [level, " ", time, " ", pid, " ", mfa, ":", line, "\n", msg, "\n"]}),

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

    %% 初始化一些不需要挂进程的东西
    local_lock:init_ets(),

    %% 子进程
    %% MYSQL
    MysqlSpecs = make_mysql_specs(),
    ?DO_IF(MysqlSpecs =/= [], virture:init_ets()),

    ChildSpecs = MysqlSpecs,

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
            {prepare, [{test, "SELECT * FROM player WHERE id=?"}]}],
        [mysql_poolboy:child_spec(virture, PoolOptions, MySqlOptions)]
    catch
        C:E ->
            ?LOG_WARNING("bad mysql config, ~p", [{C, E}]),
            []
    end.
