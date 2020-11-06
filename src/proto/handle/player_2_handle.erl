%% @private auto create
-module(player_2_handle).

-include("util.hrl").
-include("player_2_pb.hrl").
-include("player.hrl").
-include("gateway.hrl").
-include("error_code.hrl").

-export([handle/2]).

-spec handle(proto:msg(), #player{}) -> #player{}.
handle(#player_c_info{}, #player{id = Id, name = Name, gateway = Gateway} = Player) ->
    exia:cast(Gateway, ?MSG_GATEWAY_SEND_MSG1(#player_s_info{id = Id, name = Name})),
    Player;

handle(Msg, Acc) ->
    ?LOG_WARNING("unknow msg ~w", [Msg]),
    Acc.

