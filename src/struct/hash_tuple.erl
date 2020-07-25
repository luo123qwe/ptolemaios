%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(hash_tuple).
-author("dominic").

%% API
-export([new/0, new/2, lookup/2, insert/3, delete/2]).

-record(hash_tuple, {
    incr,% 每次尝试增长数组的大小
    data_size = 0,% 数据大小
    tuple_size,% tuple长度
    tuple
}).

new() ->
    new(8, 16).

new(Incr, TupleSize) ->
    #hash_tuple{incr = Incr, tuple_size = TupleSize, tuple = erlang:make_tuple(TupleSize, [])}.

lookup(Key, #hash_tuple{tuple_size = TupleSize, tuple = Tuple}) ->
    Hash = erlang:phash2(Key, TupleSize) + 1,
    case element(Hash, Tuple) of
        {_, Value} -> {ok, Value};
        _ -> error
    end.

insert(Key, Value, #hash_tuple{incr = Incr, data_size = DataSize, tuple_size = TupleSize, tuple = Tuple} = HashTuple) ->
    Hash = erlang:phash2(Key, TupleSize) + 1,
    case element(Hash, Tuple) of
        {_, _} ->
            {TupleSize1, Tuple1} = extend_hash_tuple(Incr, TupleSize, TupleSize, Tuple),
            insert(Key, Value, HashTuple#hash_tuple{tuple_size = TupleSize1, tuple = Tuple1});
        _ ->
            Tuple1 = erlang:setelement(Hash, Tuple, {Key, Value}),
            HashTuple#hash_tuple{tuple = Tuple1, data_size = DataSize + 1}
    end.

extend_hash_tuple(Incr, TupleSize, OldTupleSize, Tuple) ->
    TupleSize1 = Incr + TupleSize,
    case TupleSize1 > 1 bsl 32 of
        true -> throw(overflow);
        _ ->
            Tuple1 = erlang:make_tuple(TupleSize1, []),
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
    case element(N, OldTuple) of
        {Key, _Value} = KV ->
            Hash = erlang:phash(Key, NewTupleSize),
            case element(Hash, NewTuple) of
                {_, _} ->
                    false;
                _ ->
                    NewTuple1 = setelement(Hash, NewTuple, KV),
                    copy_hash_tuple(N - 1, OldTuple, NewTupleSize, NewTuple1)
            end;
        _ ->
            copy_hash_tuple(N - 1, OldTuple, NewTupleSize, NewTuple)
    end.

delete(Key, #hash_tuple{data_size = DataSize, tuple_size = TupleSize, tuple = Tuple} = HashTuple) ->
    Hash = erlang:phash2(Key, TupleSize) + 1,
    Tuple1 = erlang:setelement(Hash, Tuple, []),
    HashTuple#hash_tuple{tuple = Tuple1, data_size = DataSize - 1}.