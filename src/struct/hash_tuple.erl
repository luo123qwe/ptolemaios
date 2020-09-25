%%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 数据量越大, hash密度越低
%%% 测试的结果是
%%%     存储空间随着数据量增加快速上升
%%%     速度仅比dict快一点
%%%     大量数据空间换时间不划算, 少量数据list更优
%%% 结论是
%%%     跟想象不一样, 空间效率过低, 性能没啥提升
%%%     不要使用该模块
%%%     代码放着以供参考
%%% @end
%%%-------------------------------------------------------------------
-module(hash_tuple).
-author("dominic").

-include("util.hrl").

%% API
-export([new/0, size/1, tuple_size/1, new/2, find/2, store/3, erase/2, fold/3]).

-define(NOT_USE, []).
-define(KV(K, V), {K, V}).

-record(hash_tuple, {
    incr,% 每次尝试增长数组的大小
    data_size = 0,% 数据大小
    tuple_size,% tuple长度
    tuple
}).

new() ->
    new(8, 16).

new(Incr, TupleSize) ->
    #hash_tuple{incr = Incr, tuple_size = TupleSize, tuple = erlang:make_tuple(TupleSize, ?NOT_USE)}.

size(#hash_tuple{data_size = Size}) -> Size.

tuple_size(#hash_tuple{tuple_size = Size}) -> Size.

find(Key, #hash_tuple{tuple_size = TupleSize, tuple = Tuple}) ->
    Hash = erlang:phash2(Key, TupleSize) + 1,
    case erlang:element(Hash, Tuple) of
        ?KV(_, Value) -> {ok, Value};
        _ -> error
    end.

store(Key, Value, #hash_tuple{incr = Incr, data_size = DataSize, tuple_size = TupleSize, tuple = Tuple} = HashTuple) ->
    Hash = erlang:phash2(Key, TupleSize) + 1,
    case erlang:element(Hash, Tuple) of
        ?KV(Key, _OldValue) ->
            Tuple1 = erlang:setelement(Hash, Tuple, ?KV(Key, Value)),
            HashTuple#hash_tuple{tuple = Tuple1};
        ?KV(_, _) ->
            {TupleSize1, Tuple1} = extend_hash_tuple(Incr, TupleSize, TupleSize, Tuple),
            store(Key, Value, HashTuple#hash_tuple{tuple_size = TupleSize1, tuple = Tuple1});
        _ ->
            Tuple1 = erlang:setelement(Hash, Tuple, ?KV(Key, Value)),
            HashTuple#hash_tuple{tuple = Tuple1, data_size = DataSize + 1}
    end.

extend_hash_tuple(Incr, TupleSize, OldTupleSize, Tuple) ->
    TupleSize1 = Incr + TupleSize,
    case TupleSize1 > 1 bsl 32 of
        true -> throw(overflow);
        _ ->
            Tuple1 = erlang:make_tuple(TupleSize1, ?NOT_USE),
            case copy_hash_tuple(OldTupleSize, Tuple, TupleSize1, Tuple1) of
                Tuple2 when is_tuple(Tuple2) ->
                    {TupleSize1, Tuple2};
                _ ->
                    extend_hash_tuple(Incr, TupleSize1, OldTupleSize, Tuple)
            end
    end.

copy_hash_tuple(0, _OldTuple, _NewTupleSize, NewTuple) ->
    NewTuple;
copy_hash_tuple(N, OldTuple, NewTupleSize, NewTuple) ->
    case erlang:element(N, OldTuple) of
        ?KV(Key, _Value) = KV ->
            Hash = erlang:phash2(Key, NewTupleSize) + 1,
            case element(Hash, NewTuple) of
                ?KV(_, _) ->
                    false;
                _ ->
                    NewTuple1 = setelement(Hash, NewTuple, KV),
                    copy_hash_tuple(N - 1, OldTuple, NewTupleSize, NewTuple1)
            end;
        _ ->
            copy_hash_tuple(N - 1, OldTuple, NewTupleSize, NewTuple)
    end.

erase(Key, #hash_tuple{data_size = DataSize, tuple_size = TupleSize, tuple = Tuple} = HashTuple) ->
    Hash = erlang:phash2(Key, TupleSize) + 1,
    Tuple1 = erlang:setelement(Hash, Tuple, ?NOT_USE),
    HashTuple#hash_tuple{tuple = Tuple1, data_size = DataSize - 1}.

fold(Fun, Acc, #hash_tuple{tuple_size = TupleSize, tuple = Tuple}) ->
    fold(Fun, Acc, TupleSize, Tuple).

fold(_Fun, Acc, 0, _Tuple) ->
    Acc;
fold(Fun, Acc, N, Tuple) ->
    case erlang:element(N, Tuple) of
        ?KV(K, V) ->
            case Fun(K, V, Acc) of
                ?FOLD_BREAK ->
                    Acc;
                ?FOLD_BREAK_1(Acc1) ->
                    Acc1;
                Acc1 ->
                    fold(Fun, Acc1, N - 1, Tuple)
            end;
        _ ->
            fold(Fun, Acc, N - 1, Tuple)
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

base_test_() ->
    HT100 = lists:foldl(fun(N, Acc) ->
        hash_tuple:store(N, N, Acc)
                        end, hash_tuple:new(), lists:seq(1, 100)),
    
    HT80 = lists:foldl(fun(N, Acc) ->
        hash_tuple:erase(N, Acc)
                       end, HT100, lists:seq(81, 100)),
    [
        ?_assertEqual(error, hash_tuple:find(101, HT100)),
        ?_assertEqual({ok, 80}, hash_tuple:find(80, HT100)),
        
        ?_assertEqual(lists:seq(1, 100), lists:sort(hash_tuple:fold(fun(K, _, Acc) -> [K | Acc] end, [], HT100))),
        ?_assertEqual(lists:seq(1, 80), lists:sort(hash_tuple:fold(fun(K, _, Acc) -> [K | Acc] end, [], HT80)))
    ].

-endif.