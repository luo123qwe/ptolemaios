%%%-------------------------------------------------------------------
%% @doc ptolemaios public API
%% @end
%%%-------------------------------------------------------------------

-module(ptolemaios_app).

-behaviour(application).

-export([start/2, stop/1, async_stop/1]).

start(_StartType, _StartArgs) ->
    log:start(),% 日志
    ptolemaios_sup:start_link().

stop(_State) ->
    log:stop(),% 日志
    ok.

async_stop(Time) ->
    application:stop(ptolemaios),
    spawn(fun() -> timer:sleep(Time), halt() end),
    ok.
