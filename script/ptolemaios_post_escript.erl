%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ptolemaios_post_escript).
-author("dominic").

%% API
-export([main/1]).

%% ========private_split_str==========

main(_) ->
    io:format("post_hooks start~n"),
    io:format("post_hooks end~n").
