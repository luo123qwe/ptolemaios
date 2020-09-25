%% process dict
-define(PD_EXIA_SEND, pd_exia_send).% 发送的消息
-define(PD_EXIA_TIME, pd_exia_time).% 单条消息真实时间
-define(PD_EXIA_PD, pd_exia_pd).% 替代进程字典, 用来回滚
-define(PD_EXIA_ROLLBACK, pd_exia_rollback).% 回滚

%% tag
-define(EXIA_PREPARE_MSG3(After, Dest, Msg), {After, Dest, Msg}).
-define(EXIA_RETURN1(Return), {exia_private_return, Return}).% 返回

%% message
-define(MSG_EXIA_EXECUTE1(Execute), {exia_private_execute, Execute}).

%% dynamic
-define(DYM_EXIA_SEND2(M, A), apply(M, send, A)).
-define(DYM_EXIA_CB3(M, F, A), apply(M, F, A)).

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