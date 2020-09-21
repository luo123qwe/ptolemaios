%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% escript根据erl自动生成escript再执行
%%% 需要和escript在同一文件夹下
%%% @end
%%%-------------------------------------------------------------------
-module(example_escript_auto_compile).
-author("dominic").

%% API
-export([main/1]).

%% 新建同名escript, 第一行添加 "#!/usr/bin/env escript"
%% 修改main/1的make_escript("填上文件名")
%% 然后复制以下部分到escript

%% ========private_split_str==========
-include_lib("kernel/include/file.hrl").

main(Args) ->
    case make_escript("example_escript_auto_compile") of
        true ->
            escript:start(Args);
        _ ->
            do_main(Args)
    end.

do_main(_) ->
    do_something.

%% 因为不能用?MODULE
make_escript(Name) ->
    EScriptName = Name ++ ".escript",
    ErlName = Name ++ ".erl",
    {ok, #file_info{mtime = ScriptMTime}} = file:read_file_info(EScriptName),
    IsMake =
        case file:read_file_info(ErlName) of
            {ok, #file_info{mtime = ErlMTime}} when ErlMTime > ScriptMTime -> true;
            _ -> false
        end,
    case IsMake of
        true ->
            {ok, Bin} = file:read_file(ErlName),
            [_, EscriptBody] = re:split(Bin, <<"%% ========private_split_str==========">>, [{parts, 2}]),
            file:write_file(EScriptName, ["#!/usr/bin/env escript", EscriptBody]),
            io:format("make ~s~n", [EScriptName]);
        _ ->
            skip
    end,
    IsMake.