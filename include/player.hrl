%% message
-define(MSG_PLAYER_GW_MSG1(Msg), {player_gw_msg, Msg}).
-define(MSG_PLAYER_GW_DISCONNECT, player_gw_disconnect).

-record(player, {
    vt_sql_key,
    vt_sql_state,
    id :: integer(),
    account :: binary(),
    name :: binary()
}).