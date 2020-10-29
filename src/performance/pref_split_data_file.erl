%%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 分割数据文件的编译和执行效率
%%% 前提:
%%%     一个数据文件可能很大, 拆分后能否提高编译和执行效率
%%% 结论:
%%%     10W数据量, 分别尝试拆成1, 2, 4, 8, 16, 32, 128
%%%     编译效率:
%%%         A, 达到核心数时几乎没有提高
%%%         B, 未达到核心数时线性提高
%%%     执行效率
%%%         A, 按文件线性搜索时线性下降
%%%         B, 添加(分块的)条件优化时性能非常少的损失
%%% @end
%%%-------------------------------------------------------------------
-module(pref_split_data_file).
-author("dominic").

-include("perf.hrl").

%% API
-export([run/1]).

run(#performance_split_data_file{file_num = FileNum, avg_size = AvgSize, lookup_times = Times} = Performan) ->
    %% 生成文件
    make_file(FileNum, AvgSize),
    %% 编译主文件
    c:c(test_0),
    io:format("~200p~n~200p~n~200p~n", [
        lists:zip(record_info(fields, performance_split_data_file), tl(tuple_to_list(Performan))),
        {compile, test_0:test_0:compile()}, {lookup, test_0:run(Times)}
    ]).

make_file(FileNum, AvgSize) ->
    make_0(FileNum, AvgSize),
    lists:foreach(fun(FileIndex) ->
        FileName = "test_" ++ integer_to_list(FileIndex),
        Head = "-module(" ++ FileName ++ ").\n"
        "-export([get/1]).\n",
        Body =
            lists:foldl(fun(N, Acc) ->
                NStr = integer_to_list(N + (FileIndex - 1) * AvgSize),
                ["get(" ++ NStr ++ ") -> " ++ NStr ++ ";\n" | Acc]
                        end, [], lists:seq(AvgSize, 1, -1)),
        Tail = "get(_) -> undefined.\n",
        file:write_file(FileName ++ ".erl", [Head, Body, Tail])
                  end, lists:seq(1, FileNum)).

make_0(FileNum, AvgSize) ->
    Head = "-module(test_0).\n"
    "-export([get/1, run/1, compile/0]).\n"
    "get(N) ->\n",
    Get = make_0_get(1, FileNum + 1, AvgSize),
    Run = "run(N) ->
    timer:tc(fun run_1/1, [N]).
run_1(0) ->
    ok;
run_1(N) ->
    _ = test_0:get(N),
    run_1(N - 1).\n",
    Compile =
        "compile() ->\n"
        "    Self = self(),\n" ++
        "    Start = erlang:system_time(millisecond),\n" ++
        ["    spawn(fun() -> c:c(test_" ++ integer_to_list(N) ++ "), Self ! " ++ integer_to_list(N) ++ " end),\n" || N <- lists:seq(1, FileNum)] ++
        "    [receive N -> ok end || N <- lists:seq(1, " ++ integer_to_list(FileNum) ++ ")],\n"
    "    End = erlang:system_time(millisecond),\n"
    "    End - Start",
    file:write_file("test_0.erl", [Head, Get, ".\n", Run, Compile, ".\n"]).

make_0_get(N, N, _) ->
    OffSet = lists:duplicate((N - 1) * 2 + 1, "    "),
    OffSet ++ "undefined";
make_0_get(FileIndex, N, AvgSize) ->
    OffSet = lists:duplicate((FileIndex - 1) * 2 + 1, "    "),
%%    OffSet ++ "case  test_" ++ integer_to_list(FileIndex) ++ ":get(N) of\n"% 没有优化
    OffSet ++ "case " ++ integer_to_list(AvgSize * (FileIndex - 1) + 1) ++ " =< N andalso N =< " ++ integer_to_list(AvgSize * FileIndex) ++ " andalso test_" ++ integer_to_list(FileIndex) ++ ":get(N) of\n"
        ++ OffSet ++ "    undefined ->\n" ++ make_0_get(FileIndex + 1, N, AvgSize) ++ ";\n"
        ++ OffSet ++ "    Return -> Return\n"
        ++ OffSet ++ "end".