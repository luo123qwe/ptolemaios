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

main([Millisecond | Args]) ->
    {ok, hooks_escript} = dets:open_file(hooks_escript, [{file, "hooks_escript/hooks_escript.dets"}]),
    case dets:lookup(hooks_escript, ?MODULE) of
        [{_, ExecuteTime}] when ExecuteTime == Millisecond ->
            skip;
        _ ->
            dets:insert(hooks_escript, {?MODULE, Millisecond}),
            dets:close(hooks_escript),
            main_1(Args)
    end.

main_1(["clean"]) ->
    clean();
main_1(["compile"]) ->
    compile().

compile() ->
    io:format("post_hooks compile start~n"),
    io:format("post_hooks compile end~n").

clean() ->
    io:format("post_hooks clean start~n"),
    io:format("post_hooks clean end~n").