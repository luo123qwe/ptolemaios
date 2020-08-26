-module(proto_mapping).

-include("util.hrl").

-export([load/0, proto/1, encode/1, decode/2, route/2]).

-spec load() -> ok.
load() ->
  enif_protobuf:load_cache(test_0_pb:get_msg_defs()),
  enif_protobuf:load_cache(player_1_pb:get_msg_defs()),
  ok.

-spec proto(tuple()) -> error|integer().
proto(Msg) when element(1, Msg) == proto_test-> 1;
proto(Msg) when element(1, Msg) == proto_test2-> 2;
proto(Msg) when element(1, Msg) == proto_test4-> 4;
proto(Msg) when element(1, Msg) == player_c_account_login-> 101;
proto(Msg) when element(1, Msg) == player_s_account_login-> 102;
proto(Msg) when element(1, Msg) == player_c_select_role-> 103;
proto(Msg) when element(1, Msg) == player_s_select_role-> 104;
proto(Msg) when element(1, Msg) == player_c_create_role-> 105;
proto(Msg) when element(1, Msg) == player_s_create_role-> 106;
proto(_) -> error.

-spec encode(tuple()) -> {error, atom()} | binary().
encode(Msg) ->
  enif_protobuf:encode(Msg).

-spec decode(integer(), tuple()) -> {error, term()} | tuple().
decode(1, Bin) ->
  enif_protobuf:decode(Bin, proto_test);
decode(2, Bin) ->
  enif_protobuf:decode(Bin, proto_test2);
decode(4, Bin) ->
  enif_protobuf:decode(Bin, proto_test4);
decode(101, Bin) ->
  enif_protobuf:decode(Bin, player_c_account_login);
decode(102, Bin) ->
  enif_protobuf:decode(Bin, player_s_account_login);
decode(103, Bin) ->
  enif_protobuf:decode(Bin, player_c_select_role);
decode(104, Bin) ->
  enif_protobuf:decode(Bin, player_s_select_role);
decode(105, Bin) ->
  enif_protobuf:decode(Bin, player_c_create_role);
decode(106, Bin) ->
  enif_protobuf:decode(Bin, player_s_create_role);
decode(_Proto, _Bin) ->
  {error, noexist}.

-spec route(tuple(), term()) -> term().
route(Msg, Acc) when element(1, Msg) == proto_test->
  test_0_handle:handle(Msg, Acc);
route(Msg, Acc) when element(1, Msg) == proto_test2->
  test_0_handle:handle(Msg, Acc);
route(Msg, Acc) when element(1, Msg) == proto_test4->
  test_0_handle:handle(Msg, Acc);
route(Msg, Acc) when element(1, Msg) == player_c_account_login->
  player_1_handle:handle(Msg, Acc);
route(Msg, Acc) when element(1, Msg) == player_s_account_login->
  player_1_handle:handle(Msg, Acc);
route(Msg, Acc) when element(1, Msg) == player_c_select_role->
  player_1_handle:handle(Msg, Acc);
route(Msg, Acc) when element(1, Msg) == player_s_select_role->
  player_1_handle:handle(Msg, Acc);
route(Msg, Acc) when element(1, Msg) == player_c_create_role->
  player_1_handle:handle(Msg, Acc);
route(Msg, Acc) when element(1, Msg) == player_s_create_role->
  player_1_handle:handle(Msg, Acc);
route(Msg, Acc) ->
  ?LOG_WARNING("unknow msg ~w", [Msg]),
  Acc.

