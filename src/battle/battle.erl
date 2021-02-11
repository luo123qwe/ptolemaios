%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 杂项
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(battle).
-author("dominic").

-include("plm_lib.hrl").
-include("battle.hrl").

%% API
-export([get_id/1]).

-export([init_rand_seed/0, init_rand_seed/1, rand/0, rand/1]).

-export([distance/2, distance/3, distance/4]).


%% @doc 获取某个标识下的唯一id, 从1开始
-spec get_id(any()) -> non_neg_integer().
get_id(Type) ->
    case plm_svr:get(?PD14_ID1(Type)) of
        Id when is_integer(Id) -> ok;
        _ -> Id = 0
    end,
    NewId = Id + 1,
    plm_svr:put(?PD14_ID1(Type), NewId),
    NewId.

%% @doc 初始化随机状态, 使用rand.erl的算法产生种子
init_rand_seed() ->
    %% 从rand.erl复制
    Seed =
        {erlang:phash2([{node(), self()}]),
            erlang:system_time(),
            erlang:unique_integer()},
    init_rand_seed(Seed).

%% @doc 初始化随机状态, 随机数算法根据项目需求替换即可
init_rand_seed(Seed) ->
    State = rand:seed_s(exsplus, Seed),
    plm_svr:put(?PD14_RAND_STATE, State),
    Seed.

%% @doc 获取随机数, 0-1
rand() ->
    {Rand, State} = rand:uniform_s(plm_svr:get(?PD14_RAND_STATE)),
    plm_svr:put(?PD14_RAND_STATE, State),
    Rand.


%% @doc 获取随机数, 1-N
rand(N) ->
    {Rand, State} = rand:uniform_s(N, plm_svr:get(?PD14_RAND_STATE)),
    plm_svr:put(?PD14_RAND_STATE, State),
    Rand.

%% @doc 两个单位的距离
distance(#battle_unit{x = X1, y = Y1}, #battle_unit{x = X2, y = Y2}) ->
    distance(X1, Y1, X2, Y2).

%% @doc 一个点和一个单位的距离
distance(X1, Y1, #battle_unit{x = X2, y = Y2}) ->
    distance(X1, Y1, X2, Y2).

%% @doc 两个点的距离
distance(X1, Y1, X2, Y2) ->
    DX = X1 - X2,
    DY = Y1 - Y2,
    math:sqrt(DX * DX + DY * DY).


    
    