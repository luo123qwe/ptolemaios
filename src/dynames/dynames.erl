%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 杂项
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dynames).
-author("dominic").

-include("util.hrl").
-include("dynames.hrl").

%% API
-export([get_id/1]).

%% @doc 获取某个标识下的唯一id, 从1开始
-spec get_id(any()) -> non_neg_integer().
get_id(Type) ->
    case exia:eget(?PD_DYNAMES_ID1(Type)) of
        Id when is_integer(Id) -> ok;
        _ -> Id = 0
    end,
    NewId = Id + 1,
    exia:eput(?PD_DYNAMES_ID1(Type), NewId),
    NewId.