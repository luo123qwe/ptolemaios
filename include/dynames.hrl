%% 排序id
-define(DYNAMES_SORT(Frame, Priority), Frame * 10000 + Priority).

%% 帧
-record(dynames_effect, {
    sort :: integer(),% 方便排序. ?DYNAMES_SORT
    frame :: integer(),%% 帧
    priority :: integer(),%% 优先级
    effect :: any(),%% 效果
    user :: any()%% 释放者, 战斗单位的唯一id
}).

-record(dynames, {
    frame :: integer(),
    frame_list :: [#dynames_effect{}]
}).