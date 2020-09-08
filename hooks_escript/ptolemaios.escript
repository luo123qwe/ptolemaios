#!/usr/bin/env escript

-include_lib("kernel/include/file.hrl").

main(["compile"]) ->
    io:format("hooks compile~n"),
    make_escript(),
    make_config();
main(["clean"]) ->
    case is_clean() of
        true ->
            io:format("hooks clean~n"),
            make_escript(),
            clean_config();
        _ ->
            skip
    end.

%% rebar3 clean 命令会执行两次
is_clean() ->
    {ok, hooks_escript} = dets:open_file(hooks_escript, [{file, "hooks_escript/hooks_escript.dets"}]),
    case dets:lookup(hooks_escript, ptolemaios_escript) of
        [{ptolemaios_escript, OldCount}] ->
            Count = OldCount + 1;
        _ ->
            Count = 1
    end,
    dets:insert(hooks_escript, {ptolemaios_escript, Count}),
    dets:close(hooks_escript),
    (Count rem 2) == 0.

%% 创建pre和post的escript
make_escript() ->
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

%% 创建sys.config和vm.args
make_config() ->
    {ok, #file_info{mtime = SysConfigScriptMTime}} = file:read_file_info("config/sys.config.script"),
    case file:read_file_info("config/sys.config") of
        {ok, #file_info{mtime = SysConfigMTime}} when SysConfigMTime > SysConfigScriptMTime->
            skip;
        _ ->
            {ok, SysConfig} = file:script("config/sys.config.script"),
            file:write_file("config/sys.config", io_lib:format("~p.", [SysConfig]))
    end,

    {ok, #file_info{mtime = VmArgsScriptMTime}} = file:read_file_info("config/vm.args.script"),
    case file:read_file_info("config/vm.args") of
        {ok, #file_info{mtime = VmArgstMTime}} when VmArgstMTime > VmArgsScriptMTime->
            skip;
        _ ->
            {ok, VmArgs} = file:script("config/vm.args.script"),
            VmArgs1 =
                lists:map(fun(Element) ->
                    case Element of
                        {K, V} -> [K, $ , V, $\n, $\n];
                        _ -> [Element, $\n]
                    end end, VmArgs),
            file:write_file("config/vm.args", io_lib:format("~s", [VmArgs1]))
    end.

clean_config() ->
    file:delete("config/sys.config"),
    file:delete("config/vm.args").
