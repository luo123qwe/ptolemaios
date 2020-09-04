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

main(["clean"]) ->
    case is_clean() of
        true -> clean();
        _ -> ok
    end;
main(["compile"]) ->
    compile().

%% rebar3 clean 命令会执行两次
is_clean() ->
    {ok, hooks_escript} = dets:open_file(hooks_escript, [{file, "hooks_escript/hooks_escript.dets"}]),
    case dets:lookup(hooks_escript, ptolemaios_post_escript) of
        [{ptolemaios_post_escript, OldCount}] ->
            Count = OldCount + 1;
        _ ->
            Count = 1
    end,
    dets:insert(hooks_escript, {ptolemaios_post_escript, Count}),
    dets:close(hooks_escript),
    (Count rem 2) == 0.

compile() ->
    io:format("post_hooks compile start~n"),
    io:format("post_hooks compile end~n").

clean() ->
    io:format("post_hooks clean start~n"),
    io:format("post_hooks clean end~n").