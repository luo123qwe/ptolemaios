%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 前提
%%%     element系列函数效率是否与element长度有关
%%% 结论
%%%     数据长度越长, 效率越低
%%%     特别是更新, 参考源码
%%%     sys_memcpy(hp, ptr, sizeof(Eterm)*size)
%%%     似乎是因为每次更新都会复制
%%% @end
%%%-------------------------------------------------------------------
-module(performance_element).
-author("dominic").

-include("performance.hrl").

%% API
-export([
    run/1
]).

run(#performance_element{} = Performan) ->
    io:format("~200p~n~200p~n~200p~n", [
        lists:zip(record_info(fields, performance_element), tl(tuple_to_list(Performan))),
        run(Performan#performance_element.size1, Performan),
        run(Performan#performance_element.size2, Performan)
    ]).

run(Size, #performance_element{
    lookup_times = LookupTimes,
    store_times = StoreTimes
}) ->
    Tuple = erlang:make_tuple(Size, undefined),
    {{size, Size},
        {element(1, timer:tc(fun lookup/4, [LookupTimes, Size, Size, Tuple]))},
        {element(1, timer:tc(fun store/4, [StoreTimes, Size, Size, Tuple]))}
    }.

lookup(0, _, _, _) -> ok;
lookup(Times, 0, Size, Tuple) -> lookup(Times - 1, Size, Size, Tuple);
lookup(Times, N, Size, Tuple) ->
    _ = element(N, Tuple),
    lookup(Times, N - 1, Size, Tuple).

store(0, _, _, _) -> ok;
store(Times, 0, Size, Tuple) -> store(Times - 1, Size, Size, Tuple);
store(Times, N, Size, Tuple) ->
    _ = setelement(N, Tuple, undefined),
    store(Times, N - 1, Size, Tuple).