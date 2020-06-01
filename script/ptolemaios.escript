#!/usr/bin/env escript

-include("../include/util.hrl").

main(Args) ->
    io:format("Args: ~s~n", [string:join(Args, " ")]),
    halt(0).