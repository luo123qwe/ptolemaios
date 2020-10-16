%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xlsx2erl_util).
-author("dominic").

%% API
-export([binary_format/1]).

%% @doc 字符串转换成, <<"abcd"/utf8>>
binary_format(String) ->
    "<<\"" ++ String ++ "\"/utf8>>".