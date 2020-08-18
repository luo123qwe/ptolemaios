-record(account, {
    vmysql_key,
    vmysql_state,
    account :: binary(),
    id_list :: [integer()]
}).

-record(player, {
    vmysql_key,
    vmysql_state,
    id :: integer(),
    account :: binary(),
    name :: binary()
}).