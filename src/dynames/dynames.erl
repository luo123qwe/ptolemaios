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

-export([init_rand_seed/0, init_rand_seed/1, rand/0, rand/1]).

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
    exia:eput(?PD_DYNAMES_RAND_STATE, State),
    Seed.

%% @doc 获取随机数, 0-1
rand() ->
    {Rand, State} = rand:uniform_s(exia:eget(?PD_DYNAMES_RAND_STATE)),
    exia:eput(?PD_DYNAMES_RAND_STATE, State),
    Rand.


%% @doc 获取随机数, 1-N
rand(N) ->
    {Rand, State} = rand:uniform_s(N, exia:eget(?PD_DYNAMES_RAND_STATE)),
    exia:eput(?PD_DYNAMES_RAND_STATE, State),
    Rand.
    
    
    
    