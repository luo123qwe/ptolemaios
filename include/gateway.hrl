%% process dict
-define(PD_GATEWAY_C_SOCKET, pd_gateway_c_socket).% socket
-define(PD_GATEWAY_C_BIN, pd_gateway_c_bin).% bin
-define(PD_GATEWAY_C_MODULE, pd_gateway_c_module).% module

%% tag
-define(GATEWAY_LEN, 16).% 协议数据长度
-define(GATEWAY_PROTO, 16).% 协议号

%% message
-define(MSG_GATEWAY_SEND_MSG1(Msg), {gateway_send_msg, Msg}).

%% dynamic
-define(DYM_GATEWAY_C_CB3(M, F, A), erlang:apply(M, F, A)).

-record(gateway, {
    socket,
    bin = <<>>,
    account,
    player_id,
    player_pid
}).