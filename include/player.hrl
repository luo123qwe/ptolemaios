%% message
-define(MSG_PLAYER_GATEWAY_MSG1(Msg), {player_gateway_msg, Msg}).
-define(MSG_PLAYER_GATEWAY_DISCONNECT, player_gateway_disconnect).

-record(player, {
    virture_mysql_key,
    virture_mysql_state,
    id :: integer(),
    account :: binary(),
    name :: binary()
}).