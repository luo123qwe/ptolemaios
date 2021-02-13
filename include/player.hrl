-ifndef(PLAYER_HRL).
-define(PLAYER_HRL, true).

%% message
-define(MSG13_GATEWAY_PROTO1(Msg), {gateway_proto, Msg}).
-define(MSG13_GATEWAY_DISCONNECT, gateway_disconnect).
-define(MSG13_GATEWAY_RECONNECT1(Gateway), {gateway_reconnect, Gateway}).

%% marco
-define(M13_LL_PLAYER_ID1(PlayerId), {player, PlayerId}).
-define(M13_LL_ACCOUNT1(Account), {account, Account}).

%% sql
-define(M13_SQL_SELECT_ID(Account), io_lib:format("select id from player where account='~s'", [Account])).
-define(M13_SQL_CREATE2(Account, Name), io_lib:format("insert into player set account='~s', name='~s'", [Account, Name])).

-record(player, {
    plm_sql_key,
    plm_sql_state,
    id :: integer(),
    account :: binary(),
    name :: binary()
}).

-record(player_state, {
    id :: integer(),
    gateway :: pid()
}).

-endif.