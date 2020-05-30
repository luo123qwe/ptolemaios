%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 5月 2020 11:50
%%%-------------------------------------------------------------------
-module(list_util).
-author("dominic").

-include("util.hrl").

%% API
-export([fold/3]).

%% @doc 遍历列表</br>
%% 可返回?LIST_UTIL_BREAK|?LIST_UTIL_BREAK(Acc1)终止遍历
-spec fold(fun((E, Acc) -> Acc1|?UTIL_FOLD_BREAK|?UTIL_FOLD_BREAK(Acc1)), term(), list()) -> term()
    when E :: term(), Acc :: term(), Acc1 :: term().
fold(F, Acc, []) when is_function(F, 2) -> Acc;
fold(F, Acc, [H | T]) ->
    case F(H, Acc) of
        ?UTIL_FOLD_BREAK ->
            Acc;
        ?UTIL_FOLD_BREAK(Acc1) ->
            Acc1;
        Acc1 ->
            fold(F, Acc1, T)
    end.