#!/usr/bin/env escript

-include_lib("kernel/include/file.hrl").

main(_) ->
    %% 创建pre和post的escript
    PreEscript = "hooks_escript/ptolemaios_pre.escript",
    PreErl = "hooks_escript/ptolemaios_pre_escript.erl",
    do_make_escript(PreErl, PreEscript),
    PostEscript = "hooks_escript/ptolemaios_post.escript",
    PostErl = "hooks_escript/ptolemaios_post_escript.erl",
    do_make_escript(PostErl, PostEscript).

do_make_escript(Erl, Escript) ->
    IsMakePre =
        case file:read_file_info(Escript) of
            {ok, #file_info{mtime = EscriptMTime}} ->
                {ok, #file_info{mtime = ErlMTime}} = file:read_file_info(Erl),
                ErlMTime > EscriptMTime;
            _ ->
                true
        end,
    case IsMakePre of
        true ->
            {ok, Bin} = file:read_file(Erl),
            [_, EscriptBody] = re:split(Bin, <<"%% ========private_split_str==========">>),
            file:write_file(Escript, ["#!/usr/bin/env escript", EscriptBody]),
            io:format("make ~s~n", [Escript]);
        _ ->
            skip
    end.


