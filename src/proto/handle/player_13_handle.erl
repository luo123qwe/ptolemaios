%% @private auto create
-module(player_13_handle).

-include("plm_lib.hrl").
-include("game.hrl").
-include("player_13_pb.hrl").
-include("player.hrl").
-include("gateway.hrl").

-export([handle/2]).

-spec handle(proto:msg(), #player{}) -> #player{}.
handle(#player_c_info{}, #player_state{id = Id, gateway = Gateway} = Player) ->
    #player{name = Name} = plm_sql:lookup(player, [Id]),
    gateway_svr:send_proto(Gateway, #player_s_info{id = Id, name = Name}),
    Player;

handle(Msg, Acc) ->
    ?LOG_WARNING("unknow msg ~w", [Msg]),
    Acc.

