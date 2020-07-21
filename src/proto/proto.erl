%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 协议
%%% @end
%%%-------------------------------------------------------------------
-module(proto).
-author("dominic").

-include("test_0_pb.hrl").

%% API
-export([]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

base_test_() ->
    Msg = #proto_test{str = <<"str">>, ui32 = 123, rp_str = [<<"中文"/utf8>>, <<"abc123">>], rp_ui32 = [123, 456]},
    B = (catch test_0_pb:encode_msg(Msg)),
    [
        ?_assert(is_binary(B)),
        ?_assertEqual(Msg, test_0_pb:decode_msg(B, proto_test)),
        ?_test(enif_protobuf:load_cache(test_0_pb:get_msg_defs())),
        ?_assertEqual(Msg, enif_protobuf:decode(B, proto_test)),
        ?_assertEqual(B, enif_protobuf:encode(Msg))
    ].


-endif.