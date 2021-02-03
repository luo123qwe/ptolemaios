%%% @private
%%%-------------------------------------------------------------------
%% @doc ptolemaios public API
%% @end
%%%-------------------------------------------------------------------

-module(ptolemaios_app).

-behaviour(application).

-export([start/2, stop/1, async_stop/1]).

start(_StartType, _StartArgs) ->
    %% 日志
    plm_log:start(),
    %% 修复
    plm_fix_hot:system_init(),
    plm_fix_restart:system_init(),
    {ok, Pid} = ptolemaios_sup:start_link(),
    %% 初始化数据库
    vsql:system_init(ptolemaios_virture_define:get()),
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
