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

-include_lib("kernel/include/file.hrl").

main(["pre_hooks"]) ->
    io:format("pre_hooks start~n"),
    %% 读取rebar.config
    {ok, RebarConfig} = file:consult("rebar.config"),
    make_proto(RebarConfig),
    io:format("pre_hooks end~n");
main(["post_hooks"]) ->
    io:format("post_hooks start~n"),
    io:format("post_hooks end~n").

make_proto(RebarConfig) ->
    case lists:keyfind(gpb_opts, 1, RebarConfig) of
        {_, GpbOpts} -> ok;
        _ -> GpbOpts = []
    end,
    ProtoSrc = proplists:get_value(i, GpbOpts, "proto"),
    Suffix = proplists:get_value(module_name_suffix, GpbOpts, "_pb"),
    OutErlPath = proplists:get_value(o_erl, GpbOpts, "src/proto"),
    MappingFile = OutErlPath ++ "/proto_mapping.erl",
    %% 是否需要生成文件
    IsMake =
        case file:read_file_info(MappingFile) of
            {ok, #file_info{mtime = MapMTime}} ->
                filelib:fold_files(ProtoSrc, ".*\.proto", true, fun(FileName, Acc) ->
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
                "-include(\"util.hrl\").\n\n"
                "-export([load/0, encode/1, decode/2]).\n\n",
            LoadHead = "load() ->\n",
            {Load, Decode} =
                filelib:fold_files(ProtoSrc, ".*\.proto", true, fun(FileName, {L, D} = Acc) ->
                    {ok, B} = file:read_file(FileName),
                    %% 匹配所有, message name{// 12345
                    case re:run(B, "message\s*([A-z0-9_]*)\s*{//\s*([0-9]*)", [global, multiline, {capture, [1, 2], binary}]) of
                        {match, MatchList} ->
                            Name = filename:basename(FileName, ".proto") ++ Suffix,
                            L1 = ["  enif_protobuf:load_cache(", Name, ":get_msg_defs()),\n" | L],
                            D1 =
                                lists:foldr(fun([PName, Proto], Acc_1) ->
                                    ["decode(", Proto, ", Bin) ->\n  enif_protobuf:decode(Bin, ", PName, ");\n" | Acc_1]
                                            end, D, MatchList),
                            {L1, D1};
                        _ ->
                            io:format("warning bad proto ~s", [FileName]),
                            Acc
                    end
                                                               end, {[], []}),
            LoadTail = "  ok.\n\n",
            Encode =
                "encode(Msg) ->\n"
                "  enif_protobuf:encode(Msg).\n\n",
            DecodeTail =
                "decode(Proto, _Bin) ->\n"
                "  ?LOG_WARNING(\"unknow proto ~w\", [Proto]).\n\n",
            file:write_file(MappingFile, [Head, LoadHead, Load, LoadTail, Encode, Decode, DecodeTail]);
        _ ->
            skip
    end.


