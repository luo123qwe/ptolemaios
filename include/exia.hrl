-define(PD_EXIA_SEND, pd_exia_send).% 发送的消息
-define(PD_EXIA_TIME, pd_exia_time).% 单条消息真实时间
-define(PD_EXIA_PD, pd_exia_pd).% 替代进程字典, 用来回滚
-define(PD_EXIA_ROLLBACK, pd_exia_rollback).% 回滚

-define(EXIA_PREPARE_MSG(After, Dest, Msg), {After, Dest, Msg}).

%% warp一层
-record(exia_msg, {
    expect_time,
    msg
}).

%% 处理消息前保存数据
-record(exia_rollback, {
    state,
    virture,
    pd,
    send = []
}).