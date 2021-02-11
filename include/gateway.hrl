-ifndef(GATEWAY_HRL).
-define(GATEWAY_HRL, true).

%% macro
-define(M12, 12).
-define(M12_PROTO_LEN, 16).% 协议数据长度
-define(M12_PROTO_NUM, 16).% 协议号
-define(M12_PROTO_HEAD1(Proto), Proto div 100).% 协议头id

%% process dict, c = client
-define(PD12_C_SOCKET, m12_c_socket).% socket
-define(PD12_C_BIN, m12_c_bin).% bin
-define(PD12_C_MODULE, m12_c_module).% module

%% message
-define(MSG12_SEND_MSG1(Msg), {send_msg, Msg}).

%% dynamic
-define(DYM12_C_CB3(M, F, A), erlang:apply(M, F, A)).

%% gateway进程状态
-record(gateway_state, {
    socket,
    bin = <<>>,
    account,
    player_id,
    player_pid
}).

-endif.