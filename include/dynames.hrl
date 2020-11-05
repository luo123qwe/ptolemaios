-ifndef(DYNAMES_HRL).
-define(DYNAMES_HRL, true).

-define(DYM_DYNAMES_UNIT3(M, F, A), apply(M, F, A)).

-define(MSG_DYNAMES_NEXT_FRAME, dynames_next_frame).% 执行下一帧

-define(PD_DYNAMES_ID1(Type), {dynames_id, Type}).% 唯一id
-define(PD_DYNAMES_RAND_STATE, rand_seed).% 随机种子

%% 排序id
-define(DYNAMES_SORT2(Frame, Priority), -Frame * 10000 + Priority).

%% 事件
-define(DYNAMES_EVENT_TEST, test).% 测试

%% 唯一id类型
-define(DYNAMES_ID_TYPE_GLOBAL, global).

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
    actor_id :: integer(),% 配置表的角色id
    module :: atom()% 回调模块
}).

-record(dynames, {
    frame = 0 :: integer(),
    stream_event = [] :: map(),% 流事件, #{frame => [#dynames_event{}]}
    trigger_event = #{} :: map(),% 触发型事件, {type => [#dynames_event{}]}
    unit_map = maps:new() :: map()% 所有unit, #{id => #dynames_unit{}}
}).


-endif.