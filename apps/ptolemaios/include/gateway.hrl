-define(GATEWAY_LEN, 16).% 协议数据长度
-define(GATEWAY_PROTO, 16).% 协议号

-record(gateway, {
    socket,
    bin = <<>>,
    account,
    player_id,
    player_pid
}).