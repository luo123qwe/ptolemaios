%% @private auto create
-module(game_11_handle).

-include("plm_lib.hrl").
-include("game.hrl").
-include("game_11_pb.hrl").

-export([handle/2]).

-spec handle(proto:msg(), any()) -> any().
handle(Msg, Acc) ->
    ?LOG_WARNING("unknow msg ~w", [Msg]),
    Acc.

