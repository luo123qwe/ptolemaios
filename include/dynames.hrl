-define(DYM_DYNAMES_UNIT3(M, F, A), apply(M, F, A)).

-define(MSG_DYNAMES_NEXT_FRAME, dynames_next_frame).

%% 排序id
-define(DYNAMES_SORT2(Frame, Priority), -Frame * 10000 + Priority).

%% 事件
-define(DYNAMES_EVENT_TEST, test).% 测试

%% 帧
-record(dynames_event, {
    sort :: integer(),% 方便排序, 从大到小. ?DYNAMES_SORT
    frame :: integer(),%% 帧
    priority :: integer(),%% 优先级
    event :: any(),%% 事件
    user :: any(),%% 该事件发起者
    stream :: any()%% 事件的数据流
}).

%% 战斗单位
-record(dynames_unit, {
    id :: any(),
    module :: atom()% 回调模块
}).

-record(dynames, {
    frame = 0 :: integer(),
    stream_event = [] :: [#dynames_event{}],% 流事件
    trigger_event = #{} :: map(),% 触发型事件, {type => [#dynames_event{}]}
    unit_map = maps:new() :: map()
}).