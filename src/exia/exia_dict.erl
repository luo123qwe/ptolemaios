%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 5月 2020 17:57
%%%-------------------------------------------------------------------
-module(exia_dict).
-author("dominic").

-include("exia.hrl").
-include("util.hrl").

%% API
-export([new/0, lookup/2, store/3, erase/2, fold/3, fold_by_range/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 从dict.erl 复制 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% kv结构改成{key, value}, 使用 lists:keyfind/3 nif
%% value = [record], 使用 lists:keystore/4

%% Note: mk_seg/1 must be changed too if seg_size is changed.
-define(EXIA_DICT_SEG_SIZE, 16).
-define(max_seg, 32).
-define(expand_load, 5).
-define(contract_load, 3).
-define(exp_size, (?EXIA_DICT_SEG_SIZE * ?expand_load)).
-define(con_size, (?EXIA_DICT_SEG_SIZE * ?contract_load)).

-type segs(_Key, _Value) :: tuple().
-define(kv(K, V), {K, V}).            % Key-Value pair format

%% Define a hashtable.  The default values are the standard ones.
-record(exia_dict,
{
    size = 0 :: non_neg_integer(),    % Number of elements
    n = ?EXIA_DICT_SEG_SIZE :: non_neg_integer(),    % Number of active slots
    maxn = ?EXIA_DICT_SEG_SIZE :: non_neg_integer(),    % Maximum slots
    bso = ?EXIA_DICT_SEG_SIZE div 2 :: non_neg_integer(),    % Buddy slot offset
    exp_size = ?exp_size :: non_neg_integer(),    % Size to expand at
    con_size = ?con_size :: non_neg_integer(),    % Size to contract at
    empty :: tuple(),        % Empty segment
    segs :: segs(_, _)            % Segments
}).

-type dict() :: #exia_dict{}.
-export_type([dict/0]).

%% @doc 创建一个dict
-spec new() -> dict().
new() ->
    Empty = mk_seg(?EXIA_DICT_SEG_SIZE),
    #exia_dict{empty = Empty, segs = {Empty}}.


%% @doc 根据key查找
-spec lookup(term(), dict()) -> [#exia_ie{}].
lookup(Key, Dict) ->
    Slot = get_slot(Dict, Key),
    Bkt = get_bucket(Dict, Slot),
    case lists:keyfind(Key, 1, Bkt) of
        false -> [];
        {_, ElementList} -> ElementList
    end.


%% @doc 保存一个element
-spec store(#exia_ie{}|undefined, #exia_ie{}, dict()) -> dict().
store(OldElement, NewElement, Dict) ->
    case OldElement == undefined orelse OldElement#exia_ie.key == NewElement#exia_ie.key of
        true ->% 索引值没变
            store(NewElement, Dict);
        false ->
            Dict1 = erase(OldElement, Dict),
            store(NewElement, Dict1)
    end.

store(#exia_ie{key = Key, private_key = PrivateKey} = Element, D0) ->
    Slot = get_slot(D0, Key),
    {D1, Ic} = on_bucket_store(Key, PrivateKey, Element, D0, Slot),
    maybe_expand(D1, Ic).


%% @doc 删除一个element
-spec erase(#exia_ie{}, dict()) -> dict().
erase(#exia_ie{key = Key, private_key = PrivateKey}, D0) ->
    Slot = get_slot(D0, Key),
    {D1, Dc} = on_bucket_erase(Key, PrivateKey, D0, Slot),
    maybe_contract(D1, Dc).


%% get_slot(Hashdb, Key) -> Slot.
%%  Get the slot.  First hash on the new range, if we hit a bucket
%%  which has not been split use the unsplit buddy bucket.

get_slot(T, Key) ->
    H = erlang:phash(Key, T#exia_dict.maxn),
    if
        H > T#exia_dict.n -> H - T#exia_dict.bso;
        true -> H
    end.

%% get_bucket(Hashdb, Slot) -> Bucket.

get_bucket(T, Slot) -> get_bucket_s(T#exia_dict.segs, Slot).

on_bucket_store(Key, PrivateKey, Element, T, Slot) ->
    SegI = ((Slot - 1) div ?EXIA_DICT_SEG_SIZE) + 1,
    BktI = ((Slot - 1) rem ?EXIA_DICT_SEG_SIZE) + 1,
    Segs = T#exia_dict.segs,
    Seg = element(SegI, Segs),
    B0 = element(BktI, Seg),
    {B1, Res} = store_bkt_val(Key, PrivateKey, Element, B0),
    {T#exia_dict{segs = setelement(SegI, Segs, setelement(BktI, Seg, B1))}, Res}.

store_bkt_val(Key, PrivateKey, Element, [?kv(Key, ElementList) | Bkt]) ->
    ElementList1 = lists:keystore(PrivateKey, #exia_ie.private_key, ElementList, Element),
    {[?kv(Key, ElementList1) | Bkt], 0};
store_bkt_val(Key, PrivateKey, Element, [Other | Bkt0]) ->
    {Bkt1, Ic} = store_bkt_val(Key, PrivateKey, Element, Bkt0),
    {[Other | Bkt1], Ic};
store_bkt_val(Key, _PrivateKey, Element, []) -> {[?kv(Key, [Element])], 1}.

on_bucket_erase(Key, PrivateKey, T, Slot) ->
    SegI = ((Slot - 1) div ?EXIA_DICT_SEG_SIZE) + 1,
    BktI = ((Slot - 1) rem ?EXIA_DICT_SEG_SIZE) + 1,
    Segs = T#exia_dict.segs,
    Seg = element(SegI, Segs),
    B0 = element(BktI, Seg),
    {B1, Res} = erase_key(Key, PrivateKey, B0),
    {T#exia_dict{segs = setelement(SegI, Segs, setelement(BktI, Seg, B1))}, Res}.

erase_key(Key, PrivateKey, [?kv(Key, ElementList) | Bkt]) ->
    case lists:keydelete(PrivateKey, #exia_ie.private_key, ElementList) of
        [] ->
            {Bkt, 1};
        ElementList1 ->
            {[?kv(Key, ElementList1) | Bkt], 0}
    end;
erase_key(Key, PrivateKey, [E | Bkt0]) ->
    {Bkt1, Dc} = erase_key(Key, PrivateKey, Bkt0),
    {[E | Bkt1], Dc};
erase_key(_, _, []) -> {[], 0}.

%% In maybe_expand(), the variable Ic only takes the values 0 or 1,
%% but type inference is not strong enough to infer this. Thus, the
%% use of explicit pattern matching and an auxiliary function.

maybe_expand(T, 0) -> maybe_expand_aux(T, 0);
maybe_expand(T, 1) -> maybe_expand_aux(T, 1).

maybe_expand_aux(T0, Ic) when T0#exia_dict.size + Ic > T0#exia_dict.exp_size ->
    T = maybe_expand_segs(T0),            %Do we need more segments.
    N = T#exia_dict.n + 1,                %Next slot to expand into
    Segs0 = T#exia_dict.segs,
    Slot1 = N - T#exia_dict.bso,
    B = get_bucket_s(Segs0, Slot1),
    Slot2 = N,
    [B1 | B2] = rehash(B, Slot1, Slot2, T#exia_dict.maxn),
    Segs1 = put_bucket_s(Segs0, Slot1, B1),
    Segs2 = put_bucket_s(Segs1, Slot2, B2),
    T#exia_dict{size = T#exia_dict.size + Ic,
        n = N,
        exp_size = N * ?expand_load,
        con_size = N * ?contract_load,
        segs = Segs2};
maybe_expand_aux(T, Ic) -> T#exia_dict{size = T#exia_dict.size + Ic}.

maybe_expand_segs(T) when T#exia_dict.n =:= T#exia_dict.maxn ->
    T#exia_dict{maxn = 2 * T#exia_dict.maxn,
        bso = 2 * T#exia_dict.bso,
        segs = expand_segs(T#exia_dict.segs, T#exia_dict.empty)};
maybe_expand_segs(T) -> T.

maybe_contract(T, Dc) when T#exia_dict.size - Dc < T#exia_dict.con_size,
    T#exia_dict.n > ?EXIA_DICT_SEG_SIZE ->
    N = T#exia_dict.n,
    Slot1 = N - T#exia_dict.bso,
    Segs0 = T#exia_dict.segs,
    B1 = get_bucket_s(Segs0, Slot1),
    Slot2 = N,
    B2 = get_bucket_s(Segs0, Slot2),
    Segs1 = put_bucket_s(Segs0, Slot1, B1 ++ B2),
    Segs2 = put_bucket_s(Segs1, Slot2, []),    %Clear the upper bucket
    N1 = N - 1,
    maybe_contract_segs(T#exia_dict{size = T#exia_dict.size - Dc,
        n = N1,
        exp_size = N1 * ?expand_load,
        con_size = N1 * ?contract_load,
        segs = Segs2});
maybe_contract(T, Dc) -> T#exia_dict{size = T#exia_dict.size - Dc}.

maybe_contract_segs(T) when T#exia_dict.n =:= T#exia_dict.bso ->
    T#exia_dict{maxn = T#exia_dict.maxn div 2,
        bso = T#exia_dict.bso div 2,
        segs = contract_segs(T#exia_dict.segs)};
maybe_contract_segs(T) -> T.

%% get_bucket_s(Segments, Slot) -> Bucket.
%% put_bucket_s(Segments, Slot, Bucket) -> NewSegments.

get_bucket_s(Segs, Slot) ->
    SegI = ((Slot - 1) div ?EXIA_DICT_SEG_SIZE) + 1,
    BktI = ((Slot - 1) rem ?EXIA_DICT_SEG_SIZE) + 1,
    element(BktI, element(SegI, Segs)).

put_bucket_s(Segs, Slot, Bkt) ->
    SegI = ((Slot - 1) div ?EXIA_DICT_SEG_SIZE) + 1,
    BktI = ((Slot - 1) rem ?EXIA_DICT_SEG_SIZE) + 1,
    Seg = setelement(BktI, element(SegI, Segs), Bkt),
    setelement(SegI, Segs, Seg).

%% rehash(Bucket, Slot1, Slot2, MaxN) -> [Bucket1|Bucket2].
%%  Yes, we should return a tuple, but this is more fun.

rehash([?kv(Key, _Bag) = KeyBag | T], Slot1, Slot2, MaxN) ->
    [L1 | L2] = rehash(T, Slot1, Slot2, MaxN),
    case erlang:phash(Key, MaxN) of
        Slot1 -> [[KeyBag | L1] | L2];
        Slot2 -> [L1 | [KeyBag | L2]]
    end;
rehash([], _Slot1, _Slot2, _MaxN) -> [[] | []].

mk_seg(16) -> {[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []}.

%% expand_segs(Segs, EmptySeg) -> NewSegs.
%% contract_segs(Segs) -> NewSegs.
%%  Expand/contract the segment tuple by doubling/halving the number
%%  of segments.  We special case the powers of 2 upto 32, this should
%%  catch most case.  N.B. the last element in the segments tuple is
%%  an extra element containing a default empty segment.

expand_segs({B1}, Empty) ->
    {B1, Empty};
expand_segs({B1, B2}, Empty) ->
    {B1, B2, Empty, Empty};
expand_segs({B1, B2, B3, B4}, Empty) ->
    {B1, B2, B3, B4, Empty, Empty, Empty, Empty};
expand_segs({B1, B2, B3, B4, B5, B6, B7, B8}, Empty) ->
    {B1, B2, B3, B4, B5, B6, B7, B8,
        Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty};
expand_segs({B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16}, Empty) ->
    {B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16,
        Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,
        Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty};
expand_segs(Segs, Empty) ->
    list_to_tuple(tuple_to_list(Segs)
    ++ lists:duplicate(tuple_size(Segs), Empty)).

contract_segs({B1, _}) ->
    {B1};
contract_segs({B1, B2, _, _}) ->
    {B1, B2};
contract_segs({B1, B2, B3, B4, _, _, _, _}) ->
    {B1, B2, B3, B4};
contract_segs({B1, B2, B3, B4, B5, B6, B7, B8, _, _, _, _, _, _, _, _}) ->
    {B1, B2, B3, B4, B5, B6, B7, B8};
contract_segs({B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16,
    _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}) ->
    {B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16};
contract_segs(Segs) ->
    Ss = tuple_size(Segs) div 2,
    list_to_tuple(lists:sublist(tuple_to_list(Segs), 1, Ss)).


%% @doc 遍历字典
-spec fold(fun((#exia_ie{}, Acc)-> Acc1), Acc, dict()) -> Acc1
    when Acc :: term(), Acc1 :: term().
fold(F, Acc, #exia_dict{size = 0}) when is_function(F, 2) ->
    Acc;
fold(F, Acc, D) ->
    Segs = D#exia_dict.segs,
    fold_segs(F, Acc, Segs, tuple_size(Segs)).

fold_segs(F, Acc, Segs, I) when I >= 1 ->
    Seg = element(I, Segs),
    case fold_seg(F, Acc, Seg, tuple_size(Seg)) of
        ?UTIL_FOLD_BREAK(Acc1) -> Acc1;
        Acc1 -> fold_segs(F, Acc1, Segs, I - 1)
    end;
fold_segs(_F, Acc, _, 0) -> Acc.

fold_seg(F, Acc, Seg, I) when I >= 1 ->
    case fold_bucket(F, Acc, element(I, Seg)) of
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_seg(F, Acc1, Seg, I - 1)
    end;
fold_seg(_F, Acc, _, 0) -> Acc.

fold_bucket(F, Acc, [?kv(_Key, ElementList) | Bkt]) ->
    case fold_element(F, Acc, ElementList) of
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_bucket(F, Acc1, Bkt)
    end;
fold_bucket(_F, Acc, []) -> Acc.


%% @doc 根据范围遍历字典, 频繁使用的话, 同tree结构更优
-spec fold_by_range(fun((#exia_ie{}, Acc)-> Acc1), Acc, dict(), term(), term()) -> Acc1
    when Acc :: term(), Acc1 :: term().
fold_by_range(F, Acc, #exia_dict{size = 0}, _Max, _Min) when is_function(F, 2) ->
    Acc;
fold_by_range(F, Acc, D, Max, Min) ->
    Segs = D#exia_dict.segs,
    fold_segs_by_range(F, Acc, Segs, tuple_size(Segs), Max, Min).

fold_segs_by_range(F, Acc, Segs, I, Max, Min) when I >= 1 ->
    Seg = element(I, Segs),
    case fold_seg_by_range(F, Acc, Seg, tuple_size(Seg), Max, Min) of
        ?UTIL_FOLD_BREAK(Acc1) -> Acc1;
        Acc1 -> fold_segs_by_range(F, Acc1, Segs, I - 1, Max, Min)
    end;
fold_segs_by_range(_F, Acc, _, 0, _Max, _Min) -> Acc.

fold_seg_by_range(F, Acc, Seg, I, Max, Min) when I >= 1 ->
    case fold_bucket_by_range(F, Acc, element(I, Seg), Max, Min) of
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_seg_by_range(F, Acc1, Seg, I - 1, Max, Min)
    end;
fold_seg_by_range(_F, Acc, _, 0, _Max, _Min) -> Acc.

fold_bucket_by_range(F, Acc, [?kv(Key, ElementList) | Bkt], Max, Min) when Max >= Key andalso Key >= Min ->
    case fold_element(F, Acc, ElementList) of
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_bucket_by_range(F, Acc1, Bkt, Max, Min)
    end;
fold_bucket_by_range(_F, Acc, [], _Max, _Min) -> Acc.

fold_element(F, Acc, [Element | T]) ->
    case F(Element, Acc) of
        ?UTIL_FOLD_BREAK -> ?UTIL_FOLD_BREAK(Acc);
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_element(F, Acc1, T)
    end;
fold_element(_F, Acc, []) -> Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% test %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

dict_test_() ->
    D100 =
        lists:foldl(fun(N, Acc) ->
            exia_dict:store(undefined, #exia_ie{private_key = N, key = N rem 10, record = {N, N rem 10}}, Acc) end,
            exia_dict:new(), lists:seq(1, 100)),
    List100 =
        exia_dict:fold(fun(Element, Acc) ->
            [Element | Acc]
                       end, [], D100),
    D80 =
        lists:foldl(fun(N, Acc) ->
            exia_dict:erase(#exia_ie{private_key = N, key = N rem 10, record = {N, N rem 10}}, Acc)
                    end, D100, lists:seq(1, 20)),
    List80 =
        exia_dict:fold(fun(Element, Acc) ->
            [Element | Acc]
                       end, [], D80),
    [
        ?_assertEqual([#exia_ie{private_key = N, key = N rem 10, record = {N, N rem 10}} || N <- lists:seq(1, 100)],
            lists:keysort(#exia_ie.private_key, List100)),
        ?_assertEqual([#exia_ie{private_key = N, key = N rem 10, record = {N, N rem 10}} || N <- lists:seq(21, 100)],
            lists:keysort(#exia_ie.private_key, List80)),
        ?_assertEqual([#exia_ie{private_key = N * 10 + 1, key = 1, record = {N * 10 + 1, 1}} || N <- lists:seq(0, 9)],
            exia_dict:lookup(1, D100)),
        ?_assertEqual([#exia_ie{private_key = N * 10 + 1, key = 1, record = {N * 10 + 1, 1}} || N <- lists:seq(2, 9)],
            exia_dict:lookup(1, D80))
    ].

-endif.