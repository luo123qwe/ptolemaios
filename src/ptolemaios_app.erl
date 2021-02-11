%%% @private
%%%-------------------------------------------------------------------
%% @doc ptolemaios public API
%% @end
%%%-------------------------------------------------------------------

-module(ptolemaios_app).

-behaviour(application).

-export([start/2, stop/1, async_stop/1]).

start(_StartType, _StartArgs) ->
    %% 时区
    plm_time:system_init(),
    %% 日志
    plm_log:start(),
    %% 修复
    plm_fix_hot:system_init(),
    plm_fix_restart:system_init(),
    {ok, Pid} = ptolemaios_sup:start_link(),
    %% 初始化数据库
    plm_sql:system_init(game_db_define:get()),
    plm_sql:set_default_db(game_db_define:get_default_db()),
    %% 重启更新
    plm_fix_restart:fix(),
    {ok, Pid}.

stop(_State) ->
    plm_log:stop(),% 日志
    ok.

async_stop(Time) ->
    application:stop(ptolemaios),
    spawn(fun() -> timer:sleep(Time), halt() end),
    ok.
