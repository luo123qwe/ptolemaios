%%%-------------------------------------------------------------------
%% @doc ptolemaios public API
%% @end
%%%-------------------------------------------------------------------

-module(ptolemaios_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    log:start(),% 日志
    ptolemaios_sup:start_link().

stop(_State) ->
    log:stop(),% 日志
    ok.

%% internal functions
