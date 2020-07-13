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
-export([list_l/3, list_r/3, dict/3]).

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
        ?UTIL_FOLD_BREAK -> Acc;
        ?UTIL_FOLD_BREAK(Acc1) -> Acc1;
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
        ?UTIL_FOLD_BREAK -> Acc;
        ?UTIL_FOLD_BREAK(Acc1) -> Acc1;
        Acc1 -> F(H, Acc1)
    end.

list_r_1(_F, Acc, []) ->
    Acc;
list_r_1(F, Acc, [H | T]) ->
    case list_r_1(F, Acc, T) of
        ?UTIL_FOLD_BREAK -> ?UTIL_FOLD_BREAK(Acc);
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> F(H, Acc1)
    end.

%% 从dict.erl复制来的, erl版本变化可能会有问题
-record(dict, {size, nmaxn, bso, exp_size, con_size, empty, segs}).
-define(kv(K, V), [K | V]).            % Key-Value pair format
%% @doc dict:fold/3
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
        ?UTIL_FOLD_BREAK -> Acc;
        ?UTIL_FOLD_BREAK(Acc1) -> Acc1;
        Acc1 -> fold_segs(F, Acc1, Segs, I - 1)
    end;
fold_segs(F, Acc, _, 0) when is_function(F, 3) -> Acc.

fold_seg(F, Acc, Seg, I) when I >= 1 ->
    case fold_bucket(F, Acc, element(I, Seg)) of
        ?UTIL_FOLD_BREAK -> ?UTIL_FOLD_BREAK(Acc);
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_seg(F, Acc1, Seg, I - 1)
    end;
fold_seg(F, Acc, _, 0) when is_function(F, 3) -> Acc.

fold_bucket(F, Acc, [?kv(Key, Val) | Bkt]) ->
    case F(Key, Val, Acc) of
        ?UTIL_FOLD_BREAK -> ?UTIL_FOLD_BREAK(Acc);
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_bucket(F, Acc1, Bkt)
    end;
fold_bucket(F, Acc, []) when is_function(F, 3) -> Acc.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

base_test_() ->
    Seq = lists:seq(1, 1000),
    Dict = fold:list_l(fun(N, Acc) -> dict:store(N, N, Acc) end, dict:new(), Seq),
    [
        %% list_l
        ?_assertEqual(lists:reverse(Seq), fold:list_l(fun(N, Acc) -> [N | Acc] end, [], Seq)),
        ?_assertEqual(lists:seq(500, 1, -1), fold:list_l(fun(N, Acc) ->
            case N > 500 of
                true -> ?UTIL_FOLD_BREAK;
                _ -> [N | Acc]
            end end, [], Seq)),
        ?_assertEqual(lists:seq(501, 1, -1), fold:list_l(fun(N, Acc) ->
            case N > 500 of
                true -> ?UTIL_FOLD_BREAK(N);
                _ -> [N | Acc]
            end end, [], Seq)),
        
        %% list_r
        ?_assertEqual(Seq, fold:list_r(fun(N, Acc) -> [N | Acc] end, [], Seq)),
        ?_assertEqual(lists:seq(1, 500), fold:list_r(fun(N, Acc) ->
            case N > 500 of
                true -> ?UTIL_FOLD_BREAK;
                _ -> [N | Acc]
            end end, [], Seq)),
        ?_assertEqual(lists:seq(1, 501), fold:list_r(fun(N, Acc) ->
            case N > 500 of
                true -> ?UTIL_FOLD_BREAK(N);
                _ -> [N | Acc]
            end end, [], Seq)),
        
        %% dict
        ?_assertEqual(Seq, lists:sort(fold:dict(fun(N, Acc) -> [N | Acc] end, [], Dict))),
        ?_assertEqual(500, length(fold:dict(fun(N, Acc) ->
            case N > 500 of
                true -> ?UTIL_FOLD_BREAK;
                _ -> [N | Acc]
            end end, [], Dict))),
        ?_assertEqual(lists:seq(1, 501), length(fold:dict(fun(N, Acc) ->
            case N > 500 of
                true -> ?UTIL_FOLD_BREAK(N);
                _ -> [N | Acc]
            end end, [], Dict)))
    ].

-endif.