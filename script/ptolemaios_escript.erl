%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 写好复制到escript
%%% @end
%%%-------------------------------------------------------------------
-module(ptolemaios_escript).
-author("dominic").

-export([main/1]).

main(Args) ->
    io:format("Args: ~s~n", [string:join(Args, " ")]),
    halt(0).

