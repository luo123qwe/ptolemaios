%%%-------------------------------------------------------------------
%%% @author dominic
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(performan_struct).
-author("dominic").

-include("util.hrl").
-include("exia.hrl").
-include("performan.hrl").

%% API
-export([run/1]).

run(#performan_struct{} = Performan) ->
    io:format("~200p~n~200p~n~200p~n", [
        lists:zip(record_info(fields, performan_struct), tl(tuple_to_list(Performan))),
        gb_trees(Performan),
        exia_tree(Performan)
    ]).

gb_trees(#performan_struct{
    size = Size,
    record_size = RecordSize,
    build_times = BuildTimes,
    lookup_times = LookupTimes,
    update_times = UpdateTimes,
    delete_insert_times = DeleteInsertTimes,
    fold_times = FoldTimes,
    range = {Min, Max, RangeTimes},
    sub = {Start, Len, SubTimes}
}) ->
    RecordList = [erlang:make_tuple(RecordSize, N) || N <- lists:seq(1, Size)],
    Tree =
        lists:foldl(fun(N, Acc) ->
            gb_trees:insert(N, erlang:make_tuple(RecordSize, N), Acc)
                    end, gb_trees:empty(), lists:seq(1, Size)),
    {
        gb_trees,
        {build, element(1, timer:tc(fun gb_trees_build/4, [BuildTimes, RecordList, RecordList, gb_trees:empty()]))},
        {lookup, element(1, timer:tc(fun gb_trees_lookup/4, [LookupTimes, Size, Size, Tree]))},
        {update, element(1, timer:tc(fun gb_trees_update/4, [UpdateTimes, RecordList, RecordList, Tree]))},
        {delete_insert, element(1, timer:tc(fun gb_trees_delete_insert/4, [DeleteInsertTimes, RecordList, RecordList, Tree]))},
        {fold, element(1, timer:tc(fun gb_trees_fold/2, [FoldTimes, Tree]))},
        {range, element(1, timer:tc(fun gb_trees_range/4, [RangeTimes, Min, Max, Tree]))},
        {sub, element(1, timer:tc(fun gb_trees_sub/4, [SubTimes, Start, Len, Tree]))}
    }.

gb_trees_build(0, _, _, _) ->
    ok;
gb_trees_build(Times, RecordList, [], _Tree) ->
    gb_trees_build(Times - 1, RecordList, RecordList, gb_trees:empty());
gb_trees_build(Times, RecordList, [H | T], Tree) ->
    gb_trees_build(Times, RecordList, T, gb_trees:insert(element(1, H), T, Tree)).

gb_trees_lookup(0, _, _, _) ->
    ok;
gb_trees_lookup(Times, Size, 0, Tree) ->
    gb_trees_lookup(Times - 1, Size, Size, Tree);
gb_trees_lookup(Times, Size, N, Tree) ->
    _ = gb_trees:lookup(N, Tree),
    gb_trees_lookup(Times, Size, N - 1, Tree).

gb_trees_update(0, _, _, _) ->
    ok;
gb_trees_update(Times, RecordList, [], Tree) ->
    gb_trees_update(Times - 1, RecordList, RecordList, Tree);
gb_trees_update(Times, RecordList, [H | T], Tree) ->
    gb_trees_update(Times, RecordList, T, gb_trees:update(element(1, H), H, Tree)).

gb_trees_delete_insert(0, _, _, _) ->
    ok;
gb_trees_delete_insert(Times, RecordList, [], Tree) ->
    gb_trees_delete_insert(Times - 1, RecordList, RecordList, Tree);
gb_trees_delete_insert(Times, RecordList, [H | T], Tree) ->
    Key = element(1, H),
    Tree1 = gb_trees:delete(Key, Tree),
    Tree2 = gb_trees:enter(Key, H, Tree1),
    gb_trees_delete_insert(Times, RecordList, T, Tree2).

gb_trees_fold(0, _) ->
    ok;
gb_trees_fold(Times, Tree) ->
    _ = gb_trees:to_list(Tree),
    gb_trees_fold(Times - 1, Tree).

gb_trees_range(0, _, _, _) ->
    ok;
gb_trees_range(Times, Min, Max, Tree) ->
    Itor = gb_trees:iterator_from(Min, Tree),
    gb_trees_range1(Max, Itor),
    gb_trees_range(Times - 1, Min, Max, Tree).

gb_trees_range1(Max, Itor) ->
    case gb_trees:next(Itor) of
        none -> [];
        {Key, Value, Itor1} when Key =< Max ->
            [Value | gb_trees_range1(Max, Itor1)];
        _ ->
            []
    end.

gb_trees_sub(0, _, _, _) ->
    ok;
gb_trees_sub(Times, Start, Len, Tree) ->
    Itor = gb_trees:iterator(Tree),
    gb_trees_sub1(Start, Len, Itor),
    gb_trees_sub(Times - 1, Start, Len, Tree).

gb_trees_sub1(0, Len, Itor) ->
    gb_trees_sub2(Len, Itor);
gb_trees_sub1(Start, Len, Itor) ->
    case gb_trees:next(Itor) of
        none -> [];
        {_, _, Itor1} ->
            gb_trees_sub1(Start - 1, Len, Itor1)
    end.

gb_trees_sub2(0, _Itor) ->
    [];
gb_trees_sub2(Len, Itor) ->
    case gb_trees:next(Itor) of
        none -> [];
        {_, Value, Itor1} ->
            [Value | gb_trees_sub2(Len - 1, Itor1)]
    end.


exia_tree(#performan_struct{
    size = Size,
    record_size = RecordSize,
    build_times = BuildTimes,
    lookup_times = LookupTimes,
    update_times = UpdateTimes,
    delete_insert_times = DeleteInsertTimes,
    fold_times = FoldTimes,
    range = {Min, Max, RangeTimes},
    sub = {Start, Len, SubTimes}
}) ->
    RecordList = [erlang:make_tuple(RecordSize, N) || N <- lists:seq(1, Size)],
    Tree =
        lists:foldl(fun(N, Acc) ->
            exia_tree:store(undefined, #exia_ie{private_key = N, key = N, record = erlang:make_tuple(RecordSize, N)}, Acc)
                    end, exia_tree:new(), lists:seq(1, Size)),
    {
        exia_tree,
        {build, element(1, timer:tc(fun exia_tree_build/4, [BuildTimes, RecordList, RecordList, exia_tree:new()]))},
        {lookup, element(1, timer:tc(fun exia_tree_lookup/4, [LookupTimes, Size, Size, Tree]))},
        {update, element(1, timer:tc(fun exia_tree_update/4, [UpdateTimes, RecordList, RecordList, Tree]))},
        {delete_insert, element(1, timer:tc(fun exia_tree_delete_insert/5, [DeleteInsertTimes, RecordList, RecordList, Tree, 0]))},
        {fold, element(1, timer:tc(fun exia_tree_fold/2, [FoldTimes, Tree]))},
        {range, element(1, timer:tc(fun exia_tree_range/4, [RangeTimes, Min, Max, Tree]))},
        {sub, element(1, timer:tc(fun exia_tree_sub/4, [SubTimes, Start, Len, Tree]))}
    }.

exia_tree_build(0, _, _, _) ->
    ok;
exia_tree_build(Times, RecordList, [], _Tree) ->
    exia_tree_build(Times - 1, RecordList, RecordList, exia_tree:new());
exia_tree_build(Times, RecordList, [H | T], Tree) ->
    Key = element(1, H),
    exia_tree_build(Times, RecordList, T, exia_tree:store(undefined, #exia_ie{private_key = Key, key = Key, record = H}, Tree)).

exia_tree_lookup(0, _, _, _) ->
    ok;
exia_tree_lookup(Times, Size, 0, Tree) ->
    exia_tree_lookup(Times - 1, Size, Size, Tree);
exia_tree_lookup(Times, Size, N, Tree) ->
    [_] = exia_tree:lookup(N, Tree),
    exia_tree_lookup(Times, Size, N - 1, Tree).

exia_tree_update(0, _, _, _) ->
    ok;
exia_tree_update(Times, RecordList, [], Tree) ->
    exia_tree_update(Times - 1, RecordList, RecordList, Tree);
exia_tree_update(Times, RecordList, [H | T], Tree) ->
    Key = element(1, H),
    ExiaIE = #exia_ie{private_key = Key, key = Key, record = H},
    exia_tree_update(Times, RecordList, T, exia_tree:store(ExiaIE, ExiaIE, Tree)).

exia_tree_delete_insert(0, _, _, _, _) ->
    ok;
exia_tree_delete_insert(Times, RecordList, [], Tree, Offset) ->
    exia_tree_delete_insert(Times - 1, RecordList, RecordList, Tree, Offset + 1);
exia_tree_delete_insert(Times, RecordList, [H | T], Tree, Offset) ->
    Key = element(1, H),
    ExiaIE = #exia_ie{private_key = Key, key = Key + Offset, record = H},
    Tree2 = exia_tree:store(ExiaIE, ExiaIE#exia_ie{key = Key + Offset + 1}, Tree),
    exia_tree_delete_insert(Times, RecordList, T, Tree2, Offset).

exia_tree_fold(0, _) ->
    ok;
exia_tree_fold(Times, Tree) ->
    _ = exia_tree:fold(fun(#exia_ie{record = R}, Acc) -> [R | Acc] end, [], Tree),
    exia_tree_fold(Times - 1, Tree).

exia_tree_range(0, _, _, _) ->
    ok;
exia_tree_range(Times, Min, Max, Tree) ->
    _ = exia_tree:fold(fun(#exia_ie{record = R}, Acc) -> [R | Acc] end, [], Tree, Min, Max),
    exia_tree_range(Times - 1, Min, Max, Tree).

exia_tree_sub(0, _, _, _) ->
    ok;
exia_tree_sub(Times, Start, Len, Tree) ->
    _ = exia_tree:fold(fun(#exia_ie{record = R}, {S, L, Acc}) ->
        if
            S > 1 -> {S - 1, L, Acc};
            L =< 1 -> ?UTIL_FOLD_BREAK([R | Acc]);
            true -> {S, L - 1, [R | Acc]}
        end
                       end, {Start, Len, []}, Tree),
    exia_tree_sub(Times - 1, Start, Len, Tree).