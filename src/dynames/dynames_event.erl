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
-export([insert_last/2, insert_first/2]).

%% @doc 插入一个事件到事件列表
%%
%% 同优先级则放到最后面
insert_last(Event, []) ->
    [Event];
insert_last(Event, [H | _T] = L) when element(#dynames_event.sort, Event) > element(#dynames_event.sort, H) ->
    [Event | L];
insert_last(Event, [H | T]) ->
    [H | insert_last(Event, T)].


%% @doc 插入一个事件到事件列表
%%
%% 同优先级则放到最前面
insert_first(Event, []) ->
    [Event];
insert_first(Event, [H | _T] = L) when element(#dynames_event.sort, Event) >= element(#dynames_event.sort, H) ->
    [Event | L];
insert_first(Event, [H | T]) ->
    [H | insert_first(Event, T)].

