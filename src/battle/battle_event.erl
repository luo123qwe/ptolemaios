%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 战斗事件
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(battle_event).
-author("dominic").

-include("plm_lib.hrl").
-include("battle.hrl").

%% API
-export([new/2, copy/3]).

-export([insert_last/2, insert_last_list/2, insert_first/2, insert_first_list/2]).

-export([trigger/3]).

%% @doc 初始化部分消息
new(Frame, Priority) ->
    #battle_event{
        sort = ?M14_SORT2(Frame, Priority),
        id = battle:get_id(?M14_ID_TYPE_EVENT),
        frame = Frame,
        priority = Priority
    }.

%% @doc 复制一个事件, 等价于new + 对其他字段赋值
copy(Frame, Priority, Event) ->
    Event#battle_event{
        sort = ?M14_SORT2(Frame, Priority),
        id = battle:get_id(?M14_ID_TYPE_EVENT),
        frame = Frame,
        priority = Priority
    }.

%% @doc 插入一个事件到事件map
%%
%% 同优先级则放到最后面
insert_last(#battle_event{frame = Frame} = Event, FrameMap) ->
    EventList = plm_kv:lookup(Frame, FrameMap, []),
    EventList1 = insert_last_list(Event, EventList),
    plm_kv:store(Frame, EventList1, FrameMap).

insert_last_list(Event, []) ->
    [Event];
insert_last_list(Event, [H | _T] = L) when element(#battle_event.sort, Event) > element(#battle_event.sort, H) ->
    [Event | L];
insert_last_list(Event, [H | T]) ->
    [H | insert_last_list(Event, T)].


%% @doc 插入一个事件到事件map
%%
%% 同优先级则放到最前面
insert_first(#battle_event{frame = Frame} = Event, FrameMap) ->
    EventList = plm_kv:lookup(Frame, FrameMap, []),
    EventList1 = insert_first_list(Event, EventList),
    plm_kv:store(Frame, EventList1, FrameMap).

insert_first_list(Event, []) ->
    [Event];
insert_first_list(Event, [H | _T] = L) when element(#battle_event.sort, Event) >= element(#battle_event.sort, H) ->
    [Event | L];
insert_first_list(Event, [H | T]) ->
    [H | insert_first_list(Event, T)].

%% @doc 触发一个事件
trigger(Type, StreamData, Battle) ->
    EventList = plm_kv:lookup([#battle.trigger_event, Type], Battle, []),
    do_trigger(EventList, Type, StreamData, Battle).


do_trigger([], _Type, _StreamData, Battle) ->
    Battle;
do_trigger([H | T], Type, StreamData, Battle) ->
    Key = [#battle.trigger_event, Type],
    EventList = plm_kv:lookup(Key, Battle, []),
    %% 事件的回调不一定还在
    case lists:keytake(H#battle_event.id, #battle_event.id, EventList) of
        false ->
            do_trigger(T, Type, StreamData, Battle);
        {value, _, EventList1} ->
            Battle1 = plm_kv:store(Key, EventList1, Battle),
            Battle2 = battle_svr:execute_event(H#battle_event{stream = StreamData}, Battle1),
            do_trigger(T, Type, StreamData, Battle2)
    end.
    
    