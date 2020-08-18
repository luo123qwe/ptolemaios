%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(virture_mysql_sup).

-behaviour(supervisor).

-include("util.hrl").
-include("virture.hrl").

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Specs = make_mysql_specs(),
    
    %% 配置正常才开启
    ?DO_IF(Specs =/= [], virture_mysql:system_init()),
    
    {ok, {#{strategy => one_for_one,
        intensity => 1,
        period => 5},
        Specs}
    }.

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