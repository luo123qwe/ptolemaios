%% message
-define(MSG_PLAYER_GATEWAY_MSG1(Msg), {player_gateway_msg, Msg}).
-define(MSG_PLAYER_GATEWAY_DISCONNECT, player_gateway_disconnect).

%% sql
-define(SQL_PLAYER_SELECT_ID(Account), io_lib:format("select id from player where account='~s'", [Account])).
-define(SQL_PLAYER_CREATE2(Account, Name), io_lib:format("insert into player set account='~s', name='~s'", [Account, Name])).

-record(player, {
    virture_mysql_key,
    virture_mysql_state,
    id :: integer(),
    account :: binary(),
    name :: binary()
}).