%% process dict
-define(PD_GW_C_SOCKET, pd_gw_c_socket).% socket
-define(PD_GW_C_BIN, pd_gw_c_bin).% bin
-define(PD_GW_C_MODULE, pd_gw_c_module).% module

%% tag
-define(GATEWAY_LEN, 16).% 协议数据长度
-define(GATEWAY_PROTO, 16).% 协议号

%% message
-define(MSG_GW_SEND_MSG1(Msg), {gw_send_msg, Msg}).

%% dynamic
-define(DYM_GW_C_CB3(M, F, A), erlang:apply(M, F, A)).

-record(gw, {
    socket,
    bin = <<>>,
    account,
    player_id,
    player_pid
}).