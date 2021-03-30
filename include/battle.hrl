-ifndef(BATTLE_HRL).
-define(BATTLE_HRL, true).
-include("plm_lib.hrl").

-define(DYM14_UNIT3(M, F, A), apply(M, F, A)).

-define(MSG14_NEXT_FRAME, next_frame).% 执行下一帧

-define(PD14_ID1(Type), {m14_id, Type}).% 唯一id
-define(PD14_RAND_STATE, m14_rand_seed).% 随机种子

%% 排序id
-define(M14_SORT2(Frame, Priority), -Frame * 10000 + Priority).

%% 事件最大深度, 防止循环回调
-define(M14_EXECUTE_EVENT_MAX_DEEP, 10).

%% 事件, E = Event
-define(M14_E_TEST, test).% 测试
-define(M14_E_SKILL1(SkillId), {skill, SkillId}).% 释放一个技能
-define(M14_E_NORMAL_DEAD, dead).% 普通怪物死亡触发

%% 触发事件类型
%% ETTB = event type trigger before
%% ETTA = event type trigger after
-define(M14_ETTB_DEAD, b_dead).% 死亡前
-define(M14_ETTA_DEAD, a_dead).% 死亡后

%% 唯一id类型
-define(M14_ID_TYPE_EVENT, event).
-define(M14_ID_TYPE_UNIT, unit).

%% 技能id, id*100 + 第N个技能
%% id规则跟着项目换就好
-define(M14_SKILL_ID2(Id, N), Id * 100 + N).

%% unit状态
-define(M14_UNIT_STATE_ALIVE, 0).% 存活
-define(M14_UNIT_STATE_DEAD, 1).% 死亡

%% unit从回调中返回, R = Return
-define(M14_R1(Return), {battle_return, Return}).
-define(M14_THROW_R1(Return), throw(?M14_R1(Return))).
%% 这个特殊的返回对应各回调的默认skip实现
-define(M14_R_SKIP, battle_skip).
-define(M14_THROW_R_SKIP, ?M14_THROW_R1(?M14_R_SKIP)).
-define(M14_IF1(Expr), ?DO_IF(Expr, ?M14_THROW_R_SKIP)).
-define(M14_IF_NOT1(Expr, Return), ?DO_IF_NOT(Expr, ?M14_THROW_R_SKIP)).
-define(M14_MATCH2(Expr, Match, Return), ?DO_MATCH(Expr, Match, ?M14_THROW_R_SKIP)).
-define(M14_NOT_MATCH2(Expr, Match, Return), ?DO_NOT_MATCH(Expr, Match, ?M14_THROW_R_SKIP)).
%% 通用
-define(M14_IF2(Expr, Return), ?DO_IF(Expr, ?M14_THROW_R1(Return))).
-define(M14_IF_NOT2(Expr, Return), ?DO_IF_NOT(Expr, ?M14_THROW_R1(Return))).
-define(M14_MATCH3(Expr, Match, Return), ?DO_MATCH(Expr, Match, ?M14_THROW_R1(Return))).
-define(M14_NOT_MATCH3(Expr, Match, Return), ?DO_NOT_MATCH(Expr, Match, ?M14_THROW_R1(Return))).

%% 帧
-record(battle_event, {
    sort :: integer(),% 方便排序, 从大到小. ?M14_SORT
    id :: integer(),% 唯一id
    frame :: integer(),%% 帧
    priority :: integer(),%% 优先级
    event :: any(),%% 事件
    user :: any(),%% 该事件发起者
    stream :: any()%% 事件的数据流
}).

%% 战斗单位
-record(battle_unit, {
    id :: any(),% 唯一id
    data_id :: integer(),% 配置表的角色id
    state :: integer(),% 状态
    x :: integer(),% 坐标X
    y :: integer(),% 坐标Y
    attr :: map(),% 当前属性, id => value
    origin_attr :: map(),% 原始属性, id => value
    module :: atom()% 回调模块
}).

-record(battle, {
    frame = 0 :: integer(),
    stream_event = [] :: map(),% 流事件, #{frame => [#battle_event{}]}
    trigger_event = #{} :: map(),% 触发型事件, {type => [#battle_event{}]}
    unit_map = maps:new() :: map(),% 所有unit, #{id => #battle_unit{}}
    event_deep = 0 :: integer()% 事件触发深度, 防止死循环
}).

%% 死亡的事件数据流
%% esd = event stream data
-record(battle_esd_dead, {
    killer :: integer(),% 谁造成的死亡
    dead :: integer(),% 谁死亡了
    event :: #battle_event{},% 因为什么事件
    stream :: any()%% 事件的数据流
}).

%% virtue mask start data_battle_unit
%% 战斗单位
-record(data_battle_unit, {
    id,% id
    name,% 名字
    type,% 类型
    radius,% 普通攻击半径
    skill_arg_1,% 技能1参数
    skill_arg_2% 技能2参数
}).
%% virtue mask end data_battle_unit
-endif.

