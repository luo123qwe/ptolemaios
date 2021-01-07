%% message
-define(MSG_PLAYER_GATEWAY_PROTO1(Msg), {gateway_msg, Msg}).
-define(MSG_PLAYER_GATEWAY_DISCONNECT, gateway_disconnect).
-define(MSG_PLAYER_GATEWAY_RECONNECT1(Gateway), {gateway_reconnect, Gateway}).

%% sql
-define(SQL_PLAYER_SELECT_ID(Account), io_lib:format("select id from player where account='~s'", [Account])).
-define(SQL_PLAYER_CREATE2(Account, Name), io_lib:format("insert into player set account='~s', name='~s'", [Account, Name])).

-record(player, {
    vsql_key,
    vsql_state,
    id :: integer(),
    account :: binary(),
    name :: binary()
}).

-record(player_state, {
    id :: integer(),
    gateway :: pid()
}).