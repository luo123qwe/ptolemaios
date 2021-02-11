%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ptolemaios_pre_escript).
-author("dominic").

%% API
-export([main/1]).

%% ========private_split_str==========

-include_lib("kernel/include/file.hrl").

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
    clean_config(),
    clean();
main_1(["compile"]) ->
    compile_config(),
    compile().

clean_config() ->
    file:delete("config/sys.config"),
    file:delete("config/vm.args").

clean() ->
    io:format("pre_hooks clean start~n"),
    %% 读取rebar.config
    {ok, RebarConfig} = file:script("rebar.config.script"),
    clean_proto(RebarConfig),
    io:format("pre_hooks clean end~n").


%% 创建sys.config和vm.args
compile_config() ->
    {ok, #file_info{mtime = SysConfigScriptMTime}} = file:read_file_info("config/sys.config.script"),
    case file:read_file_info("config/sys.config") of
        {ok, #file_info{mtime = SysConfigMTime}} when SysConfigMTime > SysConfigScriptMTime ->
            skip;
        _ ->
            {ok, SysConfig} = file:script("config/sys.config.script"),
            file:write_file("config/sys.config", io_lib:format("~p.", [SysConfig]))
    end,

    {ok, #file_info{mtime = VmArgsScriptMTime}} = file:read_file_info("config/vm.args.script"),
    case file:read_file_info("config/vm.args") of
        {ok, #file_info{mtime = VmArgstMTime}} when VmArgstMTime > VmArgsScriptMTime ->
            skip;
        _ ->
            {ok, VmArgs} = file:script("config/vm.args.script"),
            VmArgs1 =
                lists:map(fun(Element) ->
                    case Element of
                        {K, V} -> [K, $ , V, $\n, $\n];
                        _ -> [Element, $\n, $\n]
                    end end, VmArgs),
            file:write_file("config/vm.args", io_lib:format("~s", [VmArgs1]))
    end.

compile() ->
    io:format("pre_hooks compile start~n"),
    %% 读取rebar.config
    {ok, RebarConfig} = file:script("rebar.config.script"),
    compile_proto(RebarConfig),
    io:format("pre_hooks compile end~n").

compile_proto(RebarConfig) ->
    create_dir("./include/proto"),
    case lists:keyfind(gpb_opts, 1, RebarConfig) of
        {_, GpbOpts} -> ok;
        _ -> GpbOpts = []
    end,
    ProtoSrc = proplists:get_value(i, GpbOpts, "proto"),
    Suffix = proplists:get_value(module_name_suffix, GpbOpts, "_pb"),
    OutErlPath = proplists:get_value(o_erl, GpbOpts, "src/proto/pb"),
    create_dir(OutErlPath),
    OutHandlePath = filename:dirname(OutErlPath) ++ "/handle",
    create_dir(OutHandlePath),
    MappingFile = filename:dirname(OutErlPath) ++ "/proto_mapping.erl",
    %% 是否需要生成文件
    IsMake =
        case file:read_file_info(MappingFile) of
            {ok, #file_info{mtime = MapMTime}} ->
                filelib:fold_files(ProtoSrc, ".*\.proto$", true, fun(FileName, Acc) ->
                    case Acc of
                        true ->
                            Acc;
                        _ ->
                            %% 修改过文件
                            {ok, #file_info{mtime = MTime}} = file:read_file_info(FileName),
                            MTime > MapMTime
                    end
                                                                end, false);
            _ ->
                true
        end,
    case IsMake of
        true ->
            io:format("make proto~n"),
            Head =
                "-module(proto_mapping).\n\n"
                "-include(\"plm_lib.hrl\").\n\n"
                "-export([load/0, proto/1, encode/1, decode/2, route/2]).\n\n",
            LoadHead =
                "-spec load() -> ok.\n"
                "load() ->\n"
                "    DefList =\n",
            ProtoHead =
                "-spec proto(tuple()) -> error|integer().\n",
            DecodeHead =
                "-spec decode(integer(), tuple()) -> {error, term()} | tuple().\n",
            RouteHead =
                "-spec route(tuple(), term()) -> term().\n",
            {Load, ProtoBody, Decode, Route} =
                filelib:fold_files(ProtoSrc, ".*\.proto$", true, fun(FileName, {L, PB, D, R} = Acc) ->
                    {ok, B} = file:read_file(FileName),
                    %% 匹配所有, message name{// 12345
                    case re:run(B, "message\s*([A-z0-9_]*)\s*{//\s*([0-9]*)", [global, multiline, {capture, [1, 2], binary}]) of
                        {match, MatchList} ->
                            BaseName = filename:basename(FileName, ".proto"),
                            Name = BaseName ++ Suffix,
                            [ModuleName | _] = string:split(BaseName, "_"),
                            Route = BaseName ++ "_handle",
                            RouteFile = OutHandlePath ++ "/" ++ Route ++ ".erl",
                            case filelib:is_file(RouteFile) of
                                true -> skip;
                                false -> file:write_file(RouteFile, [
                                    "%% @private auto create\n"
                                    "-module(", Route, ").\n\n"
                                    "-include(\"plm_lib.hrl\").\n"
                                    "-include(\"game.hrl\").\n"
                                    "-include(\"" ++ Name ++ ".hrl\").\n"
                                    "-include(\"" ++ ModuleName ++ ".hrl\").\n\n"
                                    "-export([handle/2]).\n\n"
                                    "-spec handle(proto:msg(), #" ++ ModuleName ++ "{}) -> #" ++ ModuleName ++ "{}.\n"
                                    "handle(Msg, Acc) ->\n"
                                    "    ?LOG_WARNING(\"unknow msg ~w\", [Msg]),\n"
                                    "    Acc.\n\n"
                                ])
                            end,
                            L1 = [["        ", Name, ":get_msg_defs()"] | L],
                            {PB1, D1, R1} =
                                lists:foldr(fun([PName, Proto], {PB_1, D_1, R_1}) ->
                                    {
                                        ["proto(Msg) when element(1, Msg) == ", PName, " -> ", Proto, ";\n" | PB_1],
                                        ["decode(", Proto, ", Bin) ->\n  enif_protobuf:decode(Bin, ", PName, ");\n" | D_1],
                                        ["route(Msg, Acc) when element(1, Msg) == ", PName, " ->\n  ", Route, ":handle(Msg, Acc);\n" | R_1]
                                    }
                                            end, {PB, D, R}, MatchList),
                            {L1, PB1, D1, R1};
                        _ ->
                            io:format("warning no proto in ~s~n", [FileName]),
                            Acc
                    end
                                                                end, {[], [], [], []}),
            Load1 = string:join(Load, " ++\n") ++ ",\n",
            LoadTail =
                "    enif_protobuf:load_cache(DefList),\n"
                "    ok.\n\n",
            ProtoTail = "proto(_) -> error.\n\n",
            Encode =
                "-spec encode(tuple()) -> {error, atom()} | binary().\n"
                "encode(Msg) ->\n"
                "    enif_protobuf:encode(Msg).\n\n",
            DecodeTail =
                "decode(_Proto, _Bin) ->\n"
                "    {error, noexist}.\n\n",
            RouteTail =
                "route(Msg, Acc) ->\n"
                "    ?LOG_WARNING(\"unknow msg ~w\", [Msg]),\n"
                "    Acc.\n\n",
            file:write_file(MappingFile, [
                Head,
                LoadHead, Load1, LoadTail,
                ProtoHead, ProtoBody, ProtoTail,
                Encode,
                DecodeHead, Decode, DecodeTail,
                RouteHead, Route, RouteTail
            ]);
        _ ->
            skip
    end.

clean_proto(RebarConfig) ->
    case lists:keyfind(gpb_opts, 1, RebarConfig) of
        {_, GpbOpts} -> ok;
        _ -> GpbOpts = []
    end,
    OutErlPath = proplists:get_value(o_erl, GpbOpts, "src/proto/pb"),
    MappingFile = filename:dirname(OutErlPath) ++ "/proto_mapping.erl",
    case filelib:is_file(MappingFile) of
        true -> file:delete(MappingFile);
        _ -> skip
    end.

create_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true -> skip;
        _ -> file:make_dir(Dir)
    end.