-ifndef(DYNAMES_HRL).
-define(DYNAMES_HRL, true).
-include("util.hrl").

-define(DYM_DYNAMES_UNIT3(M, F, A), apply(M, F, A)).

-define(MSG_DYNAMES_NEXT_FRAME, dynames_next_frame).% 执行下一帧

-define(PD_DYNAMES_ID1(Type), {dynames_id, Type}).% 唯一id
-define(PD_DYNAMES_RAND_STATE, rand_seed).% 随机种子

%% 排序id
-define(DYNAMES_SORT2(Frame, Priority), -Frame * 10000 + Priority).

%% 事件最大深度, 防止循环回调
-define(DYNAMES_EVENT_MAX_DEEP, 10).

%% 事件
-define(DYNAMES_EVENT_TEST, test).% 测试
-define(DYNAMES_EVENT_EXECUTE(SkillId), {execute, UnitIdList, Event}).% 执行一个事件, 每次取出一个id, 然后放回事件
-define(DYNAMES_EVENT_SKILL1(SkillId), {skill, SkillId}).% 释放一个技能

%% 唯一id类型
-define(DYNAMES_ID_TYPE_GLOBAL, global).

%% 技能id, id*100 + 第N个技能
%% id规则跟着项目换就好
-define(DYNAMES_SKILL_ID2(Id, N), Id * 100 + N).

%% unit状态
-define(DYNAMES_UNIT_STATE_ALIVE, 0).% 存活
-define(DYNAMES_UNIT_STATE_DEAD, 1).% 死亡

%% unit从回调中返回,
-define(DYNAMES_RETURN1(Return), {dynames_return, Return}).
-define(THROW_DYNAMES_RETURN1(Return), throw(?DYNAMES_RETURN1(Return))).
%% 这个特殊的返回对应各回调的默认skip实现
-define(DYNAMES_RETURN_SKIP, dynames_skip).
-define(THROW_DYNAMES_RETURN_SKIP, ?THROW_DYNAMES_RETURN1(?DYNAMES_RETURN_SKIP)).
-define(DYNAMES_RETURN_IF1(Expr), ?DO_IF(Expr, ?THROW_DYNAMES_RETURN_SKIP)).
-define(DYNAMES_RETURN_IF_NOT1(Expr, Return), ?DO_IF_NOT(Expr, ?THROW_DYNAMES_RETURN_SKIP)).
-define(DYNAMES_RETURN_MATCH2(Expr, Match, Return), ?DO_MATCH(Expr, Match, ?THROW_DYNAMES_RETURN_SKIP)).
-define(DYNAMES_RETURN_NOT_MATCH2(Expr, Match, Return), ?DO_NOT_MATCH(Expr, Match, ?THROW_DYNAMES_RETURN_SKIP)).
%% 通用
-define(DYNAMES_RETURN_IF2(Expr, Return), ?DO_IF(Expr, ?THROW_DYNAMES_RETURN1(Return))).
-define(DYNAMES_RETURN_IF_NOT2(Expr, Return), ?DO_IF_NOT(Expr, ?THROW_DYNAMES_RETURN1(Return))).
-define(DYNAMES_RETURN_MATCH3(Expr, Match, Return), ?DO_MATCH(Expr, Match, ?THROW_DYNAMES_RETURN1(Return))).
-define(DYNAMES_RETURN_NOT_MATCH3(Expr, Match, Return), ?DO_NOT_MATCH(Expr, Match, ?THROW_DYNAMES_RETURN1(Return))).

%% 帧
-record(dynames_event, {
    sort :: integer(),% 方便排序, 从大到小. ?DYNAMES_SORT
    id :: integer(),% 唯一id
    frame :: integer(),%% 帧
    priority :: integer(),%% 优先级
    event :: any(),%% 事件
    user :: any(),%% 该事件发起者
    stream :: any()%% 事件的数据流
}).

%% 战斗单位
-record(dynames_unit, {
    id :: any(),% 唯一id
    data_id :: integer(),% 配置表的角色id
    state :: integer(),% 状态
    x :: integer(),% 坐标X
    y :: integer(),% 坐标Y
    attr :: map(),% 当前属性, id => value
    origin_attr :: map(),% 原始属性, id => value
    module :: atom()% 回调模块
}).

-record(dynames, {
    frame = 0 :: integer(),
    stream_event = [] :: map(),% 流事件, #{frame => [#dynames_event{}]}
    trigger_event = #{} :: map(),% 触发型事件, {type => [#dynames_event{}]}
    unit_map = maps:new() :: map(),% 所有unit, #{id => #dynames_unit{}}
    event_deep = 0 :: integer()% 事件触发深度, 防止死循环
}).

%%%%%%%%%%%xlsx2erl_dynames record define start%%%%%%%%%%%%%%%%%
-record(data_dynames_unit, {id, name, type, radius, skill_arg_1, skill_arg_2}).
%%%%%%%%%%%xlsx2erl_dynames record define end%%%%%%%%%%%%%%%%%%%

-endif.