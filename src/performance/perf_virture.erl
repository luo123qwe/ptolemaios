%%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc virture mysql效率
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(perf_virture).
-author("dominic").

-include("perf.hrl").
-include("vt.hrl").

%% API
-export([
    run/0,
    run/1
]).

run() ->
    run(#performance_virture{size = 100, lookup_times = 10000, store_times = 10000, fold_times = 10000}).

run(#performance_virture{} = Performan) ->
    io:format("~200p~n~200p~n~200p~n", [
        lists:zip(record_info(fields, performance_virture), tl(tuple_to_list(Performan))),
        run_virture(Performan),
        run_dict(Performan)
    ]).


run_dict(#performance_virture{
    size = Size,
    lookup_times = LookupTimes,
    store_times = StoreTimes,
    fold_times = FoldTimes
}) ->
    RecordList = [#vt_sql_test_player{player_id = N} || N <- lists:seq(1, Size)],
    Dict =
        lists:foldl(fun(N, Acc) ->
            dict:store(N, #vt_sql_test_player{player_id = N}, Acc)
                    end, dict:new(), lists:seq(1, Size)),
    {
        dict,
        {lookup, element(1, timer:tc(fun dict_lookup/4, [LookupTimes, Size, Size, Dict]))},
        {store, element(1, timer:tc(fun dict_store/4, [StoreTimes, RecordList, RecordList, Dict]))},
        {fold, element(1, timer:tc(fun dict_fold/2, [FoldTimes, Dict]))}
    }.

dict_lookup(0, _, _, _) ->
    ok;
dict_lookup(LookupTimes, 0, Size, Dict) ->
    dict_lookup(LookupTimes - 1, Size, Size, Dict);
dict_lookup(LookupTimes, N, Size, Dict) ->
    _ = dict:find(N, Dict),
    dict_lookup(LookupTimes, N - 1, Size, Dict).

dict_store(0, _, _, _) ->
    ok;
dict_store(StoreTimes, [], RecordList, Dict) ->
    dict_store(StoreTimes - 1, RecordList, RecordList, Dict);
dict_store(StoreTimes, [Record | T], RecordList, Dict) ->
    Dict1 = dict:store(Record#vt_sql_test_player{}, Record, Dict),
    dict_store(StoreTimes, T, RecordList, Dict1).

dict_fold(0, _Dict) ->
    ok;
dict_fold(N, Dict) ->
    _ = dict:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], Dict),
    dict_fold(N - 1, Dict).

run_virture(#performance_virture{
    size = Size,
    lookup_times = LookupTimes,
    store_times = StoreTimes,
    fold_times = FoldTimes
}) ->
    vt_sql:process_init(),
    vt_sql:clean_pd(),
    vt_sql:load(vt_sql_test_player, undefined),
    RecordList = [#vt_sql_test_player{player_id = N} || N <- lists:seq(1, Size)],
    lists:foreach(fun(N) ->
        vt_sql:insert(#vt_sql_test_player{player_id = N})
                  end, lists:seq(1, Size)),
    {
        virture,
        {lookup, element(1, timer:tc(fun virture_lookup/3, [LookupTimes, Size, Size]))},
        {store, element(1, timer:tc(fun virture_store/3, [StoreTimes, RecordList, RecordList]))},
        {fold, element(1, timer:tc(fun virture_fold/1, [FoldTimes]))}
    }.


virture_lookup(0, _, _) ->
    ok;
virture_lookup(LookupTimes, 0, Size) ->
    virture_lookup(LookupTimes - 1, Size, Size);
virture_lookup(LookupTimes, N, Size) ->
    _ = vt_sql:lookup(vt_sql_test_player, [N]),
    virture_lookup(LookupTimes, N - 1, Size).

virture_store(0, _, _) ->
    ok;
virture_store(StoreTimes, [], RecordList) ->
    virture_store(StoreTimes - 1, RecordList, RecordList);
virture_store(StoreTimes, [Record | T], RecordList) ->
    vt_sql:insert(Record),
    virture_store(StoreTimes, T, RecordList).

virture_fold(0) ->
    ok;
virture_fold(N) ->
    _ = vt_sql:fold_cache(fun(K, V, Acc) -> [{K, V} | Acc] end, [], vt_sql_test_player),
    virture_fold(N - 1).

