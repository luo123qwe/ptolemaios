-module(test_0_handle).

-include("util.hrl").

-export([handle/2]).

handle(Msg, Acc) ->
  ?LOG_WARNING("unknow msg ~w", [Msg]),
  Acc.

