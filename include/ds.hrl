-ifndef(DS_HRL).
-define(DS_HRL, true).
-include("util.hrl").

-define(DYM_DS_UNIT3(M, F, A), apply(M, F, A)).

-define(MSG_DS_NEXT_FRAME, ds_next_frame).% 执行下一帧

-define(PD_DS_ID1(Type), {ds_id, Type}).% 唯一id
-define(PD_DS_RAND_STATE, rand_seed).% 随机种子

%% 排序id
-define(DS_SORT2(Frame, Priority), -Frame * 10000 + Priority).

%% 事件最大深度, 防止循环回调
-define(DS_EXECUTE_EVENT_MAX_DEEP, 10).

%% 事件
-define(DS_E_TEST, test).% 测试
-define(DS_E_SKILL1(SkillId), {skill, SkillId}).% 释放一个技能
-define(DS_E_NORMAL_DEAD, dead).% 普通怪物死亡触发

%% 触发事件类型
-define(DS_ETTB_DEAD, b_dead).% 死亡前
-define(DS_ETTA_DEAD, a_dead).% 死亡后

%% 唯一id类型
-define(DS_ID_TYPE_EVENT, event).
-define(DS_ID_TYPE_UNIT, unit).

%% 技能id, id*100 + 第N个技能
%% id规则跟着项目换就好
-define(DS_SKILL_ID2(Id, N), Id * 100 + N).

%% unit状态
-define(DS_UNIT_STATE_ALIVE, 0).% 存活
-define(DS_UNIT_STATE_DEAD, 1).% 死亡

%% unit从回调中返回,
-define(DS_R1(Return), {ds_return, Return}).
-define(THROW_DS_R1(Return), throw(?DS_R1(Return))).
%% 这个特殊的返回对应各回调的默认skip实现
-define(DS_R_SKIP, ds_skip).
-define(THROW_DS_R_SKIP, ?THROW_DS_R1(?DS_R_SKIP)).
-define(DS_R_IF1(Expr), ?DO_IF(Expr, ?THROW_DS_R_SKIP)).
-define(DS_R_IF_NOT1(Expr, Return), ?DO_IF_NOT(Expr, ?THROW_DS_R_SKIP)).
-define(DS_R_MATCH2(Expr, Match, Return), ?DO_MATCH(Expr, Match, ?THROW_DS_R_SKIP)).
-define(DS_R_NOT_MATCH2(Expr, Match, Return), ?DO_NOT_MATCH(Expr, Match, ?THROW_DS_R_SKIP)).
%% 通用
-define(DS_R_IF2(Expr, Return), ?DO_IF(Expr, ?THROW_DS_R1(Return))).
-define(DS_R_IF_NOT2(Expr, Return), ?DO_IF_NOT(Expr, ?THROW_DS_R1(Return))).
-define(DS_R_MATCH3(Expr, Match, Return), ?DO_MATCH(Expr, Match, ?THROW_DS_R1(Return))).
-define(DS_R_NOT_MATCH3(Expr, Match, Return), ?DO_NOT_MATCH(Expr, Match, ?THROW_DS_R1(Return))).

%% 帧
-record(ds_event, {
    sort :: integer(),% 方便排序, 从大到小. ?DYNAMES_SORT
    id :: integer(),% 唯一id
    frame :: integer(),%% 帧
    priority :: integer(),%% 优先级
    event :: any(),%% 事件
    user :: any(),%% 该事件发起者
    stream :: any()%% 事件的数据流
}).

%% 战斗单位
-record(ds_u, {
    id :: any(),% 唯一id
    data_id :: integer(),% 配置表的角色id
    state :: integer(),% 状态
    x :: integer(),% 坐标X
    y :: integer(),% 坐标Y
    attr :: map(),% 当前属性, id => value
    origin_attr :: map(),% 原始属性, id => value
    module :: atom()% 回调模块
}).

-record(ds, {
    frame = 0 :: integer(),
    stream_event = [] :: map(),% 流事件, #{frame => [#ds_event{}]}
    trigger_event = #{} :: map(),% 触发型事件, {type => [#ds_event{}]}
    unit_map = maps:new() :: map(),% 所有unit, #{id => #ds_unit{}}
    event_deep = 0 :: integer()% 事件触发深度, 防止死循环
}).

%% 死亡的事件数据流
-record(ds_esd_dead, {
    killer :: integer(),% 谁造成的死亡
    dead :: integer(),% 谁死亡了
    event :: #ds_event{},% 因为什么事件
    stream :: any()%% 事件的数据流
}).

%% xlsx2erl mask start data_ds_u
%% 战斗单位
-record(data_ds_u, {
    id,% 
    name,% 
    type,% 
    radius,% 
    skill_arg_1,% 
    skill_arg_2% 
}).
%% xlsx2erl mask end data_ds_u
-endif.

