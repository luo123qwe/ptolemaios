%%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 前提
%%%     hash_tuple效率是否更高
%%% 结论
%%%     仅find(element/2)时效率高
%%%     空间效率极低
%%%     更新效率极低, 这里引出了performance_element
%%%     该实现不能使用
%%% @end
%%%-------------------------------------------------------------------
-module(perf_hash_tuple).
-author("dominic").

-include("perf.hrl").

%% API
-export([run/1, prof/0]).


%% 如果用prof会显示hash_tuple比dict快, 不是很懂
prof() ->
    RandList = get_rand_kv_list(64),
    Dict = lists:foldl(fun({K, V}, Acc) ->
        hash_tuple:store(K, V, Acc)
                       end, hash_tuple:new(), RandList),
%%    Dict = lists:foldl(fun({K, V}, Acc) ->
%%        dict:store(K, V, Acc)
%%                       end, dict:new(), RandList),
    eprof:start(),
    eprof:start_profiling([self()]),
    hash_tuple_store(10000, RandList, RandList, Dict),
%%    dict_store(10000, RandList, RandList, Dict),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop().

run(#performance_hash_tuple{} = Performan) ->
    io:format("~200p~n~200p~n~200p~n~200p~n", [
        lists:zip(record_info(fields, performance_hash_tuple), tl(tuple_to_list(Performan))),
        dict(Performan),
        hash_tuple(Performan),
        list(Performan)
    ]).

dict(#performance_hash_tuple{
    size = Size,
    build_times = BuildTimes,
    lookup_times = LookupTimes,
    store_times = StoreTimes,
    fold_times = FoldTimes
}) ->
    RandList = get_rand_kv_list(Size),
    Dict = lists:foldl(fun({K, V}, Acc) ->
        dict:store(K, V, Acc)
                       end, dict:new(), RandList),
    {dict,
        {size, erts_debug:size(Dict)},
        {build, element(1, timer:tc(fun dict_build/4, [BuildTimes, RandList, RandList, dict:new()]))},
        {lookup, element(1, timer:tc(fun dict_lookup/4, [LookupTimes, RandList, RandList, Dict]))},
        {store, element(1, timer:tc(fun dict_store/4, [StoreTimes, RandList, RandList, Dict]))},
        {fold, element(1, timer:tc(fun dict_fold/2, [FoldTimes, Dict]))}
    }.

dict_build(0, _, _, _Dict) ->
    ok;
dict_build(N, [], RandList, Dict) ->
    dict_build(N - 1, RandList, RandList, Dict);
dict_build(N, [{K, V} | T], RandList, Dict) ->
    dict_build(N, T, RandList, dict:store(K, V, Dict)).

dict_lookup(0, _, _, _Dict) ->
    ok;
dict_lookup(N, [], RandList, Dict) ->
    dict_lookup(N - 1, RandList, RandList, Dict);
dict_lookup(N, [{K, _V} | T], RandList, Dict) ->
    _ = dict:find(K, Dict),
    dict_lookup(N, T, RandList, Dict).

dict_store(0, _, _, _Dict) ->
    ok;
dict_store(N, [], RandList, Dict) ->
    dict_store(N - 1, RandList, RandList, Dict);
dict_store(N, [{K, V} | T], RandList, Dict) ->
    Dict1 = dict:store(K, V, Dict),
    dict_store(N, T, RandList, Dict1).

dict_fold(0, _Dict) ->
    ok;
dict_fold(N, Dict) ->
    dict:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], Dict),
    dict_fold(N - 1, Dict).

hash_tuple(#performance_hash_tuple{
    size = Size,
    build_times = BuildTimes,
    lookup_times = LookupTimes,
    store_times = StoreTimes,
    fold_times = FoldTimes
}) ->
    RandList = get_rand_kv_list(Size),
    Dict = lists:foldl(fun({K, V}, Acc) ->
        hash_tuple:store(K, V, Acc)
                       end, hash_tuple:new(), RandList),
    {hash_tuple,
        {size, erts_debug:size(Dict)},
        {build, element(1, timer:tc(fun hash_tuple_build/4, [BuildTimes, RandList, RandList, hash_tuple:new()]))},
        {lookup, element(1, timer:tc(fun hash_tuple_lookup/4, [LookupTimes, RandList, RandList, Dict]))},
        {store, element(1, timer:tc(fun hash_tuple_store/4, [StoreTimes, RandList, RandList, Dict]))},
        {fold, element(1, timer:tc(fun hash_tuple_fold/2, [FoldTimes, Dict]))}
    }.

hash_tuple_build(0, _, _, _Dict) ->
    ok;
hash_tuple_build(N, [], RandList, Dict) ->
    hash_tuple_build(N - 1, RandList, RandList, Dict);
hash_tuple_build(N, [{K, V} | T], RandList, Dict) ->
    hash_tuple_build(N, T, RandList, hash_tuple:store(K, V, Dict)).

hash_tuple_lookup(0, _, _, _Dict) ->
    ok;
hash_tuple_lookup(N, [], RandList, Dict) ->
    hash_tuple_lookup(N - 1, RandList, RandList, Dict);
hash_tuple_lookup(N, [{K, _V} | T], RandList, Dict) ->
    _ = hash_tuple:find(K, Dict),
    hash_tuple_lookup(N, T, RandList, Dict).

hash_tuple_store(0, _, _, _Dict) ->
    ok;
hash_tuple_store(N, [], RandList, Dict) ->
    hash_tuple_store(N - 1, RandList, RandList, Dict);
hash_tuple_store(N, [{K, V} | T], RandList, Dict) ->
    Dict1 = hash_tuple:store(K, V, Dict),
    hash_tuple_store(N, T, RandList, Dict1).

hash_tuple_fold(0, _Dict) ->
    ok;
hash_tuple_fold(N, Dict) ->
    hash_tuple:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], Dict),
    hash_tuple_fold(N - 1, Dict).

list(#performance_hash_tuple{
    size = Size,
    build_times = BuildTimes,
    lookup_times = LookupTimes,
    store_times = StoreTimes,
    fold_times = FoldTimes
}) ->
    RandList = get_rand_kv_list(Size),
    Dict = lists:foldl(fun({K, V}, Acc) ->
        [{K, V} | Acc]
                       end, [], RandList),
    {list,
        {size, erts_debug:size(Dict)},
        {build, element(1, timer:tc(fun list_build/4, [BuildTimes, RandList, RandList, []]))},
        {lookup, element(1, timer:tc(fun list_lookup/4, [LookupTimes, RandList, RandList, Dict]))},
        {store, element(1, timer:tc(fun list_store/4, [StoreTimes, RandList, RandList, Dict]))},
        {fold, element(1, timer:tc(fun list_fold/2, [FoldTimes, Dict]))}
    }.

list_build(0, _, _, _Dict) ->
    ok;
list_build(N, [], RandList, Dict) ->
    list_build(N - 1, RandList, RandList, Dict);
list_build(N, [{K, V} | T], RandList, Dict) ->
    list_build(N, T, RandList, [{K, V} | Dict]).

list_lookup(0, _, _, _Dict) ->
    ok;
list_lookup(N, [], RandList, Dict) ->
    list_lookup(N - 1, RandList, RandList, Dict);
list_lookup(N, [{K, _V} | T], RandList, Dict) ->
    _ = lists:keyfind(K, 1, Dict),
    list_lookup(N, T, RandList, Dict).

list_store(0, _, _, _Dict) ->
    ok;
list_store(N, [], RandList, Dict) ->
    list_store(N - 1, RandList, RandList, Dict);
list_store(N, [{K, V} | T], RandList, Dict) ->
    Dict1 = lists:keystore(K, 1, Dict, {K, V}),
    list_store(N, T, RandList, Dict1).

list_fold(0, _Dict) ->
    ok;
list_fold(N, Dict) ->
    fold:list_l(fun({K, V}, Acc) -> [{K, V} | Acc] end, [], Dict),
    list_fold(N - 1, Dict).


get_rand_kv_list(N) ->
    case get(?PD_PERFORMANCE_HASH_TUPLE_RAND_LIST) of
        {N, List} ->
            List;
        _ ->
            get_rand_kv_list(N, N, [])
    end.

get_rand_kv_list(0, Size, List) ->
    put(?PD_PERFORMANCE_HASH_TUPLE_RAND_LIST, {Size, List}),
    List;
get_rand_kv_list(N, Size, List) ->
    get_rand_kv_list(N - 1, Size, [{rand:uniform(1 bsl 32 - 1), N} | List]).