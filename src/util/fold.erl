%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 遍历, 提供break
%%% @end
%%%-------------------------------------------------------------------
-module(fold).
-author("dominic").

-include("util.hrl").

%% API
-export([list_l/3, list_r/3, dict/3, maps/3, gb_trees_l/3, gb_trees_r/3, gb_trees_l/4, gb_trees_r/4]).

%% @doc lists:foldl/3
-spec list_l(Fun, Acc0, List) -> Acc1 when
    Fun :: fun((Elem :: T, AccIn) -> AccOut),
    Acc0 :: term(),
    Acc1 :: term(),
    AccIn :: term(),
    AccOut :: term(),
    List :: [T],
    T :: term().
list_l(F, Acc, []) when is_function(F, 2) ->
    Acc;
list_l(F, Acc, [H | T]) ->
    case F(H, Acc) of
        ?FOLD_BREAK -> Acc;
        ?FOLD_BREAK_1(Acc1) -> Acc1;
        Acc1 -> list_l(F, Acc1, T)
    end.

%% @doc lists:foldr/3
-spec list_r(Fun, Acc0, List) -> Acc1 when
    Fun :: fun((Elem :: T, AccIn) -> AccOut),
    Acc0 :: term(),
    Acc1 :: term(),
    AccIn :: term(),
    AccOut :: term(),
    List :: [T],
    T :: term().
list_r(F, Acc, []) when is_function(F, 2) ->
    Acc;
list_r(F, Acc, [H | T]) ->
    case list_r_1(F, Acc, T) of
        ?FOLD_BREAK -> Acc;
        ?FOLD_BREAK_1(Acc1) -> Acc1;
        Acc1 -> F(H, Acc1)
    end.

list_r_1(_F, Acc, []) ->
    Acc;
list_r_1(F, Acc, [H | T]) ->
    case list_r_1(F, Acc, T) of
        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc);
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 ->
            case F(H, Acc1) of
                ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc1);
                ?FOLD_BREAK_1(_Acc2) = Break -> Break;
                Acc2 -> Acc2
            end
    end.

-record(dict, {size, n, maxn, bso, exp_size, con_size, empty, segs}).
-define(kv(K, V), [K | V]).            % Key-Value pair format
%% @doc dict:fold/3, 
%% 从dict.erl复制来的, erl版本变化可能会有问题, ?PTOLEMAIOS_MASK_COPY_FROM_ERL
-spec dict(Fun, Acc0, Dict) -> Acc1 when
    Fun :: fun((K :: T, V :: T, AccIn) -> AccOut),
    Acc0 :: term(),
    Acc1 :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Dict :: #dict{},
    T :: term().
dict(F, Acc, D) -> fold_dict(F, Acc, D).

fold_dict(F, Acc, #dict{size = 0}) when is_function(F, 3) ->
    Acc;
fold_dict(F, Acc, D) ->
    Segs = D#dict.segs,
    fold_segs(F, Acc, Segs, tuple_size(Segs)).

fold_segs(F, Acc, Segs, I) when I >= 1 ->
    Seg = element(I, Segs),
    case fold_seg(F, Acc, Seg, tuple_size(Seg)) of
        ?FOLD_BREAK -> Acc;
        ?FOLD_BREAK_1(Acc1) -> Acc1;
        Acc1 -> fold_segs(F, Acc1, Segs, I - 1)
    end;
fold_segs(F, Acc, _, 0) when is_function(F, 3) -> Acc.

fold_seg(F, Acc, Seg, I) when I >= 1 ->
    case fold_bucket(F, Acc, element(I, Seg)) of
        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc);
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 -> fold_seg(F, Acc1, Seg, I - 1)
    end;
fold_seg(F, Acc, _, 0) when is_function(F, 3) -> Acc.

fold_bucket(F, Acc, [?kv(Key, Val) | Bkt]) ->
    case F(Key, Val, Acc) of
        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc);
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 -> fold_bucket(F, Acc1, Bkt)
    end;
fold_bucket(F, Acc, []) when is_function(F, 3) -> Acc.

%% @doc maps:fold, 
%% 从maps.erl复制来的, erl版本变化可能会有问题, ?PTOLEMAIOS_MASK_COPY_FROM_ERL
-spec maps(Fun, Init, Map) -> Acc when
    Fun :: fun((Key, Value, AccIn) -> AccOut),
    Init :: term(),
    Acc :: AccOut,
    AccIn :: Init | AccOut,
    Map :: #{Key => Value}.

maps(Fun, Init, Map) when is_function(Fun, 3), is_map(Map) ->
    maps_1(Fun, Init, maps:iterator(Map)).

maps_1(Fun, Acc, Iter) ->
    case maps:next(Iter) of
        {K, V, NextIter} ->
            case Fun(K, V, Acc) of
                ?FOLD_BREAK -> Acc;
                ?FOLD_BREAK_1(Acc1) -> Acc1;
                Acc1 -> maps_1(Fun, Acc1, NextIter)
            end;
        none ->
            Acc
    end.


%% @doc 实际上似乎不太需要前后序遍历, 所以仅提供中序遍历, 
%% gb_trees, 左中右, 从小到大, 
%% 从gb_trees.erl复制来的, erl版本变化可能会有问题, ?PTOLEMAIOS_MASK_COPY_FROM_ERL
-spec gb_trees_l(Fun, Acc0, Tree) -> Acc1 when
    Fun :: fun((K :: T, V :: T, AccIn) -> AccOut),
    Acc0 :: term(),
    Acc1 :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Tree :: gb_trees:tree(),
    T :: term().
gb_trees_l(F, Acc, {_, T}) when is_function(F, 3) ->
    case gb_trees_l_1(F, Acc, T) of
        ?FOLD_BREAK_1(Acc1) -> Acc1;
        Acc1 -> Acc1
    end.

gb_trees_l_1(F, Acc, {Key, Value, Small, Big}) ->
    case gb_trees_l_1(F, Acc, Small) of
        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc);
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 ->
            case F(Key, Value, Acc1) of
                ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc1);
                ?FOLD_BREAK_1(_Acc2) = Break -> Break;
                Acc2 ->
                    case gb_trees_l_1(F, Acc2, Big) of
                        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc2);
                        ?FOLD_BREAK_1(_Acc3) = Break -> Break;
                        Acc3 -> Acc3
                    end
            end
    end;
gb_trees_l_1(_F, Acc, nil) -> Acc.

%% @doc gb_trees, 左中右, 从小到大, 从Key >= Start开始遍历
-spec gb_trees_l(Fun, Start, Acc0, Tree) -> Acc1 when
    Fun :: fun((K :: T, V :: T, AccIn) -> AccOut),
    Start :: term(),
    Acc0 :: term(),
    Acc1 :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Tree :: gb_trees:tree(),
    T :: term().
gb_trees_l(F, Start, Acc, {_, T}) when is_function(F, 3) ->
    case gb_trees_l_1(F, Start, Acc, T) of
        ?FOLD_BREAK_1(Acc1) -> Acc1;
        Acc1 -> Acc1
    end.

gb_trees_l_1(F, Start, Acc, {Key, _Value, _Small, Big}) when Key < Start ->
    gb_trees_l_1(F, Start, Acc, Big);
gb_trees_l_1(F, Start, Acc, {Key, Value, Small, Big}) ->
    case gb_trees_l_1(F, Start, Acc, Small) of
        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc);
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 ->
            case F(Key, Value, Acc1) of
                ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc1);
                ?FOLD_BREAK_1(_Acc2) = Break -> Break;
                Acc2 ->
                    case gb_trees_l_1(F, Start, Acc2, Big) of
                        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc2);
                        ?FOLD_BREAK_1(_Acc3) = Break -> Break;
                        Acc3 -> Acc3
                    end
            end
    end;
gb_trees_l_1(_F, _Start, Acc, nil) -> Acc.

%% @doc gb_trees, 右中左, 从大到小
-spec gb_trees_r(Fun, Acc0, Tree) -> Acc1 when
    Fun :: fun((K :: T, V :: T, AccIn) -> AccOut),
    Acc0 :: term(),
    Acc1 :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Tree :: gb_trees:tree(),
    T :: term().
gb_trees_r(F, Acc, {_, T}) when is_function(F, 3) ->
    case gb_trees_r_1(F, Acc, T) of
        ?FOLD_BREAK_1(Acc1) -> Acc1;
        Acc1 -> Acc1
    end.

gb_trees_r_1(F, Acc, {Key, Value, Small, Big}) ->
    case gb_trees_r_1(F, Acc, Big) of
        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc);
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 ->
            case F(Key, Value, Acc1) of
                ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc1);
                ?FOLD_BREAK_1(_Acc2) = Break -> Break;
                Acc2 ->
                    case gb_trees_r_1(F, Acc2, Small) of
                        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc2);
                        ?FOLD_BREAK_1(_Acc3) = Break -> Break;
                        Acc3 -> Acc3
                    end
            end
    end;
gb_trees_r_1(_F, Acc, nil) -> Acc.


%% @doc gb_trees, 右中左, 从大到小, 从Key `=<' Start开始遍历
-spec gb_trees_r(Fun, Start, Acc0, Tree) -> Acc1 when
    Fun :: fun((K :: T, V :: T, AccIn) -> AccOut),
    Start :: term(),
    Acc0 :: term(),
    Acc1 :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Tree :: gb_trees:tree(),
    T :: term().
gb_trees_r(F, Start, Acc, {_, T}) when is_function(F, 3) ->
    case gb_trees_r_1(F, Start, Acc, T) of
        ?FOLD_BREAK_1(Acc1) -> Acc1;
        Acc1 -> Acc1
    end.

gb_trees_r_1(F, Start, Acc, {Key, _Value, Small, _Big}) when Key > Start ->
    gb_trees_r_1(F, Start, Acc, Small);
gb_trees_r_1(F, Start, Acc, {Key, Value, Small, Big}) ->
    case gb_trees_r_1(F, Start, Acc, Big) of
        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc);
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 ->
            case F(Key, Value, Acc1) of
                ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc1);
                ?FOLD_BREAK_1(_Acc2) = Break -> Break;
                Acc2 ->
                    case gb_trees_r_1(F, Start, Acc2, Small) of
                        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc2);
                        ?FOLD_BREAK_1(_Acc3) = Break -> Break;
                        Acc3 -> Acc3
                    end
            end
    end;
gb_trees_r_1(_F, _Start, Acc, nil) -> Acc.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

base_test_() ->
    Seq = lists:seq(1, 1000),
    Dict = fold:list_l(fun(N, Acc) -> dict:store(N, N, Acc) end, dict:new(), Seq),
    Map = fold:list_l(fun(N, Acc) -> Acc#{N => N} end, #{}, Seq),
    GBTree = fold:list_l(fun(N, Acc) -> gb_trees:insert(N, N, Acc) end, gb_trees:empty(), Seq),
    [
        %% list_l
        ?_assertEqual(lists:reverse(Seq), fold:list_l(fun(N, Acc) -> [N | Acc] end, [], Seq)),
        ?_assertEqual(lists:seq(500, 1, -1), fold:list_l(fun(N, Acc) ->
            case N > 500 of
                true -> ?FOLD_BREAK;
                _ -> [N | Acc]
            end end, [], Seq)),
        ?_assertEqual(lists:seq(501, 1, -1), fold:list_l(fun(N, Acc) ->
            case N > 500 of
                true -> ?FOLD_BREAK_1([N | Acc]);
                _ -> [N | Acc]
            end end, [], Seq)),
        
        %% list_r
        ?_assertEqual(Seq, fold:list_r(fun(N, Acc) -> [N | Acc] end, [], Seq)),
        ?_assertEqual(lists:seq(500, 1000), fold:list_r(fun(N, Acc) ->
            case N < 500 of
                true -> ?FOLD_BREAK;
                _ -> [N | Acc]
            end end, [], Seq)),
        ?_assertEqual(lists:seq(499, 1000), fold:list_r(fun(N, Acc) ->
            case N < 500 of
                true -> ?FOLD_BREAK_1([N | Acc]);
                _ -> [N | Acc]
            end end, [], Seq)),
        
        %% dict
        ?_assertEqual(Seq, lists:sort(fold:dict(fun(_, N, Acc) -> [N | Acc] end, [], Dict))),
        ?_assertEqual([], fold:dict(fun(_, _N, _Acc) -> ?FOLD_BREAK end, [], Dict)),
        ?_assertEqual(500, fold:dict(fun(_, N, Acc) ->
            case N == 500 of
                true -> ?FOLD_BREAK_1(N);
                _ -> Acc
            end end, [], Dict)),
        
        %% maps
        ?_assertEqual(Seq, lists:sort(fold:maps(fun(_, N, Acc) -> [N | Acc] end, [], Map))),
        ?_assertEqual([], fold:maps(fun(_, _N, _Acc) -> ?FOLD_BREAK end, [], Map)),
        ?_assertEqual(500, fold:maps(fun(_, N, Acc) ->
            case N == 500 of
                true -> ?FOLD_BREAK_1(N);
                _ -> Acc
            end end, [], Map)),
        
        %% gb_trees_l
        ?_assertEqual(lists:reverse(Seq), fold:gb_trees_l(fun(_, N, Acc) -> [N | Acc] end, [], GBTree)),
        ?_assertEqual(lists:seq(500, 1, -1), fold:gb_trees_l(fun(_, N, Acc) ->
            case N > 500 of
                true -> ?FOLD_BREAK;
                _ -> [N | Acc]
            end end, [], GBTree)),
        ?_assertEqual(lists:seq(501, 1, -1), fold:gb_trees_l(fun(_, N, Acc) ->
            case N > 500 of
                true -> ?FOLD_BREAK_1([N | Acc]);
                _ -> [N | Acc]
            end end, [], GBTree)),
        
        %% gb_trees_r
        ?_assertEqual(Seq, fold:gb_trees_r(fun(_, N, Acc) -> [N | Acc] end, [], GBTree)),
        ?_assertEqual(lists:seq(500, 1000), fold:gb_trees_r(fun(_, N, Acc) ->
            case N < 500 of
                true -> ?FOLD_BREAK;
                _ -> [N | Acc]
            end end, [], GBTree)),
        ?_assertEqual(lists:seq(499, 1000), fold:gb_trees_r(fun(_, N, Acc) ->
            case N < 500 of
                true -> ?FOLD_BREAK_1([N | Acc]);
                _ -> [N | Acc]
            end end, [], GBTree)),
        
        %% gb_trees_l/4
        ?_assertEqual(lists:reverse(lists:seq(500, 1000)),
            fold:gb_trees_l(fun(_, N, Acc) ->
                [N | Acc]
                            end, 500, [], GBTree)),
        ?_assertEqual(lists:seq(500, 200, -1), fold:gb_trees_l(fun(_, N, Acc) ->
            case N > 500 of
                true -> ?FOLD_BREAK;
                _ -> [N | Acc]
            end end, 200, [], GBTree)),
        ?_assertEqual(lists:seq(501, 200, -1), fold:gb_trees_l(fun(_, N, Acc) ->
            case N > 500 of
                true -> ?FOLD_BREAK_1([N | Acc]);
                _ -> [N | Acc]
            end end, 200, [], GBTree)),
        
        %% gb_trees_r/4
        ?_assertEqual(lists:seq(1, 500), fold:gb_trees_r(fun(_, N, Acc) -> [N | Acc] end, 500, [], GBTree)),
        ?_assertEqual(lists:seq(500, 700), fold:gb_trees_r(fun(_, N, Acc) ->
            case N < 500 of
                true -> ?FOLD_BREAK;
                _ -> [N | Acc]
            end end, 700, [], GBTree)),
        ?_assertEqual(lists:seq(499, 700), fold:gb_trees_r(fun(_, N, Acc) ->
            case N < 500 of
                true -> ?FOLD_BREAK_1([N | Acc]);
                _ -> [N | Acc]
            end end, 700, [], GBTree))
    ].

-endif.