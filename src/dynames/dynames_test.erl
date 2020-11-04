%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dynames_test).
-author("dominic").

-behaviour(dynames_unit).

-include("util.hrl").
-include("dynames.hrl").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([filter_event_target/3, execute_event/4]).

filter_event_target(_Unit, Event, #dynames{unit_map = UnitMap}) ->
    %% 除自己以外的unit都是目标
    TargetList = maps:keys(UnitMap) -- [Event#dynames_event.user],
    %% 再执行两次后停止, 模拟事件的数据传递
    case Event#dynames_event.stream of
        2 ->
            {ok, TargetList, Event#dynames_event{stream = stop}};
        undefined ->
            {ok, TargetList, Event#dynames_event{stream = 1}};
        Stream ->
            {ok, TargetList, Event#dynames_event{stream = Stream + 1}}
    end.

execute_event(TargetList, Unit, Event, #dynames{frame = Frame, stream_event = StreamEvent} = State) ->
    ?debugFmt("~n~p ~p ~p~n", [TargetList, Unit, Event]),
    case Event#dynames_event.stream of
        stop ->
            %% 下一帧执行一个事件, 模拟事件延时触发
            NewEvent = dynames_event:copy(Frame + 1, Event#dynames_event.priority, Event),
            {ok, State#dynames{stream_event = dynames_event:insert_first(NewEvent#dynames_event{stream = undefined}, StreamEvent)}};
        _ ->
            %% 换成target发起, 模拟A事件触发B事件
            [TargetId | _] = TargetList,
            {ok, dynames_svr:execute_event(Event#dynames_event{user = TargetId}, State)}
    end.


-ifdef(TEST).

base_test_() ->
    {setup,
        fun() ->
            {ok, Pid} = dynames_svr:start([test], []),
            sys:replace_state(Pid, fun(_) ->
                BaseEvent1 = dynames_event:new(1, 1),
                Event1 = BaseEvent1#dynames_event{user = 1, event = ?DYNAMES_EVENT_TEST},
                BaseEvent2 = dynames_event:new(3, 1),
                Event2 = BaseEvent2#dynames_event{user = 2, event = ?DYNAMES_EVENT_TEST},
                #dynames{
                    stream_event = #{
                        1 => [Event1],
                        %% 这个事件不会执行
                        3 => [Event2]
                    },
                    unit_map = #{1 => #dynames_unit{id = 1, module = dynames_test}, 2 => #dynames_unit{id = 1, module = dynames_test}}
                }
                                   end),
            Pid
        end,
        fun(_) -> ok end,
        fun(Pid) ->
            ?_test(begin
                       Pid ! ?MSG_DYNAMES_NEXT_FRAME,
                       Pid ! ?MSG_DYNAMES_NEXT_FRAME
                   end)
        end
    }.

-endif.