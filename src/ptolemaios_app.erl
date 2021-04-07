%%% @private
%%%-------------------------------------------------------------------
%% @doc ptolemaios public API
%% @end
%%%-------------------------------------------------------------------

-module(ptolemaios_app).

-behaviour(application).

-include("plm_lib.hrl").

-export([start/2, stop/1, async_stop/1]).

start(_StartType, _StartArgs) ->
    %% 协议
    proto_mapping:load(),
    %% 时区
    plm_time:system_init(),
    %% 日志
    {ok, LogConfigList} = application:get_env(ptolemaios, log),
    {_, LogLevel} = lists:keyfind(level, 1, LogConfigList),
    {_, LogDir} = lists:keyfind(log_dir, 1, LogConfigList),
    plm_log:system_init(LogLevel, LogDir),
    %% 初始化数据库
    plm_sql_init(),
    %% 修复
    {ok, FixConfigList} = application:get_env(ptolemaios, fix),
    {_, FixDetsDir} = lists:keyfind(dets_dir, 1, FixConfigList),
    plm_fix:system_init(FixDetsDir),
    {ok, Pid} = ptolemaios_sup:start_link(),
    %% 日志
    plm_log:system_start(),
    %% 数据库
    plm_sql:system_start(game_db:get_plm_sql_cfg(plm_sql:get_default_db(undefined))),
    %% 重启更新
    plm_fix_restart:fix(),
    %% 检查数据库dets是否已经修复
    plm_sql:check_dets(),
    {ok, Pid}.

stop(_State) ->
    plm_log:stop(),% 日志
    ok.

async_stop(Time) ->
    application:stop(ptolemaios),
    spawn(fun() -> timer:sleep(Time), halt() end),
    ok.

plm_sql_init() ->
    %% 在配置里面拿到游戏数据库
    {ok, ConfigList} = application:get_env(ptolemaios, plm_db),
    {_, DBConfigList} = lists:keyfind(db_list, 1, ConfigList),
    DefaultDB =
        lists:foldl(fun(DBConfig, Acc) ->
            case lists:keyfind(type, 1, DBConfig) of
                {_, game} -> list_to_atom(plm_kv:lookup([database], DBConfig, undefined));
                _ -> Acc
            end
                    end, false, DBConfigList),
    ?DO_IF(DefaultDB == false, exit(badarg)),
    {_, DetsDir} = lists:keyfind(dets_dir, 1, ConfigList),
    PoolArgs = [{size, 50}, {max_overflow, 100}],
    DBConfigList1 =
        lists:map(fun(DBConfig) ->
            DB = list_to_atom(plm_kv:lookup([database], DBConfig, undefined)),
            {DB, PoolArgs, plm_kv:delete(type, DBConfig)}
                  end, DBConfigList),
    plm_sql:system_init(DefaultDB, DetsDir, DBConfigList1).