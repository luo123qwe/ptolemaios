%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 战斗事件
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dynames_event).
-author("dominic").

-include("util.hrl").
-include("dynames.hrl").

%% API
-export([new/2, copy/3]).

-export([insert_last/2, insert_last_list/2, insert_first/2, insert_first_list/2]).

%% @doc 初始化部分消息
new(Frame, Priority) ->
    #dynames_event{
        sort = ?DYNAMES_SORT2(Frame, Priority),
        id = dynames:get_id(?DYNAMES_ID_TYPE_GLOBAL),
        frame = Frame,
        priority = Priority
    }.

%% @doc 复制一个事件, 等价于new + 对其他字段赋值
copy(Frame, Priority, Event) ->
    Event#dynames_event{
        sort = ?DYNAMES_SORT2(Frame, Priority),
        id = dynames:get_id(?DYNAMES_ID_TYPE_GLOBAL),
        frame = Frame,
        priority = Priority
    }.

%% @doc 插入一个事件到事件map
%%
%% 同优先级则放到最后面
insert_last(#dynames_event{frame = Frame} = Event, FrameMap) ->
    EventList = kv_op:lookup(Frame, FrameMap, []),
    EventList1 = insert_last_list(Event, EventList),
    kv_op:store(Frame, EventList1, FrameMap).

insert_last_list(Event, []) ->
    [Event];
insert_last_list(Event, [H | _T] = L) when element(#dynames_event.sort, Event) > element(#dynames_event.sort, H) ->
    [Event | L];
insert_last_list(Event, [H | T]) ->
    [H | insert_last_list(Event, T)].


%% @doc 插入一个事件到事件map
%%
%% 同优先级则放到最前面
insert_first(#dynames_event{frame = Frame} = Event, FrameMap) ->
    EventList = kv_op:lookup(Frame, FrameMap, []),
    EventList1 = insert_first_list(Event, EventList),
    kv_op:store(Frame, EventList1, FrameMap).

insert_first_list(Event, []) ->
    [Event];
insert_first_list(Event, [H | _T] = L) when element(#dynames_event.sort, Event) >= element(#dynames_event.sort, H) ->
    [Event | L];
insert_first_list(Event, [H | T]) ->
    [H | insert_first_list(Event, T)].

