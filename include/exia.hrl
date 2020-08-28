-define(PD_EXIA_DEST, pd_exia_dist).% 发送的消息默认接收进程
-define(PD_EXIA_SEND, pd_exia_send).% 发送的消息
-define(PD_EXIA_EXPECT_TIME, pd_exia_msg_time).% 单条消息期望时间
-define(PD_EXIA_MSG_TIME, pd_exia_time).% 单条消息真实时间
-define(PD_EXIA_PD, pd_exia_pd).% 替代进程字典, 用来回滚
-define(PD_EXIA_ROLLBACK, pd_exia_rollback).% 回滚

-define(EXIA_PREPARE_MSG(Dest, ExpectTime, Msg), {Dest, ExpectTime, Msg}).
-define(EXIA_PREPARE_MSG(After, Dest, ExpectTime, Msg), {After, Dest, ExpectTime, Msg}).

%% warp一层
-record(exia_msg, {
    expect_time,
    msg
}).

%% 处理消息前保存数据
-record(exia_rollback, {
    state,
    virture,
    dest,
    pd,
    send = []
}).