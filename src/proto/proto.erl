%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 协议
%%%
%%% ```
%%% 编写规则
%%% 首先, {模块}与Overview中"规范"中定义一致
%%%
%%% 协议号设计
%%%     协议号使用16位无符号整数
%%%     协议号 = {模块}编号*100 + seq_id, {模块}编号范围1-654
%%%     意思是项目支持654个模块, 其中每个模块下100个协议
%%%     为什么只有100个协议, 因为我认为单个模块协议超过100条的话拆分一下会更好
%%%
%%% 协议文件名
%%%     {模块}_{模块}编号.proto, 带上编号为了方便增删查, 实际上是没什么影响的
%%%
%%% 有协议号的协议
%%%     s代表服务端发送, c代表客户端发送
%%%     message {模块}_[s|c]_[tag] {// 协议号
%%%         .....
%%%     }
%%%
%%% 无协议号的协议, 用于数据嵌套
%%%     message {模块}_p_[tag] {
%%%         .....
%%%     }'''
%%%
%%% 可参考proto/player.proto
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