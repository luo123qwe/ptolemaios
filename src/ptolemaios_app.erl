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
    log:start(),
    %% 修复
    fix_hot:system_init(),
    fix_restart:system_init(),
    {ok, Pid} = ptolemaios_sup:start_link(),
    %% 重启更新
    fix_restart:fix(),
    {ok, Pid}.

stop(_State) ->
    log:stop(),% 日志
    ok.

async_stop(Time) ->
    application:stop(ptolemaios),
    spawn(fun() -> timer:sleep(Time), halt() end),
    ok.
