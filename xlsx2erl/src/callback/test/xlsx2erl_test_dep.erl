%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 仅测试依赖, 没有实际的表
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xlsx2erl_test_dep).
-author("dominic").

%% API
-export([compile/1, clean/1]).

compile(_) ->
    %% 依赖goods数据
    Goods = xlsx2erl_test_goods:get_index(),
    io:format("xlsx2erl_test_dep xlsx2erl_test goods keys:~n~p~n", [maps:keys(Goods)]).

clean(_) ->
    ok.