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

-export([trigger/3]).

%% @doc 初始化部分消息
new(Frame, Priority) ->
    #dynames_event{
        sort = ?DYNAMES_SORT2(Frame, Priority),
        id = dynames:get_id(?DYNAMES_ID_TYPE_EVENT),
        frame = Frame,
        priority = Priority
    }.

%% @doc 复制一个事件, 等价于new + 对其他字段赋值
copy(Frame, Priority, Event) ->
    Event#dynames_event{
        sort = ?DYNAMES_SORT2(Frame, Priority),
        id = dynames:get_id(?DYNAMES_ID_TYPE_EVENT),
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

%% @doc 触发一个事件
trigger(Type, StreamData, Dynames) ->
    EventList = kv_op:lookup([#dynames.trigger_event, Type], Dynames, []),
    do_trigger(EventList, Type, StreamData, Dynames).


do_trigger([], _Type, _StreamData, Dynames) ->
    Dynames;
do_trigger([H | T], Type, StreamData, Dynames) ->
    Key = [#dynames.trigger_event, Type],
    EventList = kv_op:lookup(Key, Dynames, []),
    %% 事件的回调不一定还在
    case lists:keytake(H#dynames_event.id, #dynames_event.id, EventList) of
        false ->
            do_trigger(T, Type, StreamData, Dynames);
        {value, _, EventList1} ->
            Dynames1 = kv_op:store(Key, EventList1, Dynames),
            Dynames2 = dynames_svr:execute_event(H#dynames_event{stream = StreamData}, Dynames1),
            do_trigger(T, Type, StreamData, Dynames2)
    end.
    
    