%% @private auto create
-module(player_2_handle).

-include("util.hrl").
-include("player_2_pb.hrl").
-include("player.hrl").

-export([handle/2]).

-spec handle(proto:msg(), #player{}) -> #player{}.
handle(Msg, Acc) ->
    ?LOG_WARNING("unknow msg ~w", [Msg]),
    Acc.

