-module(xlsx2erl).

-include("xlsx2erl.hrl").
-include_lib("kernel/include/file.hrl").

%% private
-export([main/1, get_sheet_data/2, make_template/1, make_template/2]).

%%====================================================================
%% API functions
%%====================================================================

%% @private escript 自动执行export文件下所有的erl文件
main(["compile", XlsxDir0, ExportDir0]) ->
    ExportDir = filename:absname(ExportDir0),
    XlsxDir = filename:absname(XlsxDir0),
    file:make_dir(ExportDir),
    file:make_dir(?DETS_PATH),
    
    %% 需要重新生成的xlsx和模块列表
    {UpdateDetsList, ErlChangeList} =
        filelib:fold_files(XlsxDir, ".xlsx", true,
            fun(FileName, {XCL, ECL} = Acc) ->
                BaseName = filename:basename(FileName),
                case string:split(filename:rootname(BaseName), "-") of
                    [_, TagStr] ->
                        ModuleStr = "xlsx2erl_" ++ TagStr,
                        Module = list_to_atom(ModuleStr),
                        xlsx2erl_util:ensure_dets(Module),
                        case dets:lookup(?DETS_XLSX2ERL1(Module), ?XLSX2ERL_EXCEL_UPDATE_TIME) of
                            [#xlsx2erl_dets{v = UpdateTime}] ->
                                {ok, #file_info{mtime = MTime}} = file:read_file_info(FileName),
                                case UpdateTime < MTime of
                                    true ->% xlsx变了
                                        {[{Module, BaseName, FileName} | XCL], ECL};
                                    _ ->
                                        {ok, #file_info{mtime = ErlMTime}} = file:read_file_info(?XLSX2ERL_CALLBACK_PATH ++ "/" ++ ModuleStr ++ ".erl"),
                                        case UpdateTime < ErlMTime of
                                            true -> {XCL, [{Module, BaseName} | ECL]};% .erl变了
                                            _ -> Acc
                                        end
                                end;
                            _ ->
                                {[{Module, BaseName, FileName} | XCL], ECL}
                        end;
                    _ ->
                        io:format("bad file name ~ts", [BaseName]),
                        throw(error)
                end
            end, {[], []}),
    
    %% 基础设置
    erlang:process_flag(trap_exit, true),
    %% [{processor, [CoreInfo]}], Cpu数量
    CpuNum = length(element(2, hd(erlang:system_info(cpu_topology)))),
    %% 多进程更新dets
    compile_update_dets(UpdateDetsList, [], CpuNum, CpuNum),
    
    %% 构造依赖关系, 哪些模块(excel表)需要重新生成
    {ok, CallbackFileList} = file:list_dir(?XLSX2ERL_CALLBACK_PATH),
    DependModuleList = compile_make_depend(CallbackFileList, UpdateDetsList, []),
    
    %% 多进程调用对应compile函数
    CallbackArgs = #xlsx2erl_callback_args{export_path = ExportDir},
    compile_do_compile(ErlChangeList ++ DependModuleList, CallbackArgs, [], CpuNum, CpuNum),
    %% 关闭所有dets
    close_dets();
main(["clean", XlsxDir0, ExportDir0]) ->
    ExportDir = filename:absname(ExportDir0),
    XlsxDir = filename:absname(XlsxDir0),
    file:make_dir(ExportDir),
    filelib:fold_files(XlsxDir, ".xlsx", true,
        fun(FileName, _) ->
            BaseName = filename:basename(FileName),
            case string:split(filename:rootname(BaseName), "-") of
                [_, ModuleStr] ->
                    Module = list_to_atom("xlsx2erl_" ++ ModuleStr),
                    Args = #xlsx2erl_callback_args{
                        export_path = ExportDir
                    },
                    Start = erlang:system_time(millisecond),
                    case catch Module:clean(Args) of
                        {'EXIT', Error} ->
                            case code:is_loaded(Module) of
                                false ->
                                    %% 没有这个erlang文件
                                    io:format("warning clean ~ts no erl file~n", [BaseName]);
                                _ ->
                                    io:format("clean ~ts error~n~p~n~n", [BaseName, Error])
                            end;
                        _ ->
                            End = erlang:system_time(millisecond),
                            io:format("clean ~ts success, cost ~p ms~n", [BaseName, End - Start])
                    end;
                _ ->
                    io:format("bad file name ~ts", [BaseName])
            end
        end, []),
    close_dets();
main(["template", FileName, OutFile]) ->
    make_template(FileName, OutFile);
main(_) ->
    io:format(
        "compile: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" compile xlsx export\n"
        "clean: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" clean xlsx export\n"
        "template: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" template xlsx/T测试-test.xlsx .").

%% 多进程编译
compile_update_dets([], [], _, _) ->
    ok;
compile_update_dets(CompileList, ReceiveList, SpawnNum, ProcessNum) when CompileList == [] orelse SpawnNum == 0 ->
    compile_receive_spawn(ReceiveList),
    compile_update_dets(CompileList, [], ProcessNum, ProcessNum);
compile_update_dets([{Module, BaseName, FileName} | T], ReceiveList, SpawnNum, ProcessNum) ->
    Pid = spawn_link(fun() ->
        Start = erlang:system_time(millisecond),
        case (catch Module:update_dets(FileName)) of
            {'EXIT', {undef, [{Module, update_dets, [FileName], _} | _]}} ->
                %% 没有这个erlang文件, 自动创建模板
                make_template(FileName),
                io:format("make_template ~p~n", [Module]),
                close_dets(),
                throw(error);
            {'EXIT', Error} ->
                io:format("update_dets ~p error~n~p~n~n", [Module, Error]),
                close_dets(),
                file:delete(?DETS_PATH ++ "/" ++ atom_to_list(Module)),
                throw(error);
            _ ->
                End = erlang:system_time(millisecond),
                io:format("update dets ~ts, cost ~p ms~n", [BaseName, End - Start])
        end
                     end),
    compile_update_dets(T, [Pid | ReceiveList], SpawnNum - 1, ProcessNum).

%% 多进程编译
compile_do_compile([], _, [], _, _) ->
    ok;
compile_do_compile(CompileList, CallbackArgs, ReceiveList, SpawnNum, ProcessNum) when CompileList == [] orelse SpawnNum == 0 ->
    compile_receive_spawn(ReceiveList),
    compile_do_compile(CompileList, CallbackArgs, [], ProcessNum, ProcessNum);
compile_do_compile([{Module, BaseName} | T], CallbackArgs, ReceiveList, SpawnNum, ProcessNum) ->
    Pid = spawn_link(fun() ->
        Start = erlang:system_time(millisecond),
        case catch Module:compile(CallbackArgs) of
            {'EXIT', Error} ->
                io:format("compile ~ts error~n~p~n~n", [BaseName, Error]);
            _ ->
                End = erlang:system_time(millisecond),
                io:format("compile ~ts success, cost ~p ms~n", [BaseName, End - Start])
        end
                     end),
    compile_do_compile(T, CallbackArgs, [Pid | ReceiveList], SpawnNum - 1, ProcessNum).

compile_receive_spawn(ReceiveList) ->
    AnyError =
        lists:any(fun(_) ->
            receive
                {'EXIT', Pid, Reason} ->
                    case lists:member(Pid, ReceiveList) andalso Reason == normal of
                        true ->
                            false;
                        _ ->
                            io:format("compile_update_dets unknow exit ~p~n", [Reason]),
                            true
                    end;
                UnKnow ->
                    io:format("compile_update_dets unknow ~p~n", [UnKnow]),
                    true
            end
                  end, ReceiveList),
    case AnyError of
        true -> throw(error);
        _ -> ok
    end.

%% 定义:'依赖'总是通过 A表的Key==B表的Key 实现, 类似mysql的外键
%% 如果A文件被B文件依赖, B文件被C文件依赖(物品被装备依赖, 装备被装备属性依赖)
%% 当A文件改变时(物品), 同时重新生成被'直接'依赖的文件(装备), 但不会生成'间接'依赖的文件(装备属性)
%% 例如, A表数据变了, B/C表不变
%% 那么A表的key可能变了, 导致B表的key对应不上, 所以需要重新生成
%% 但是C表的key依赖B表的key, B表并没有改变, 所以不需要生成
compile_make_depend([], _CompileList, ModuleList) ->
    lists:ukeysort(1, ModuleList);
compile_make_depend([ErlFileName | T], CompileList, ModuleList) ->
    Module = list_to_atom(filename:basename(ErlFileName, ".erl")),
    case lists:keyfind(Module, 1, CompileList) of
        {_, BaseName, _} ->
            compile_make_depend(T, CompileList, [{Module, BaseName} | ModuleList]);
        _ ->
            {ok, Binary} = file:read_file(?XLSX2ERL_CALLBACK_PATH ++ "/" ++ ErlFileName),
            %% 正则找出所有 xlsx2erl_xxx: 的调用
            case re:run(Binary, "(xlsx2erl_(?!util)[A-z0-9]*):", [global, {capture, all_but_first, binary}]) of
                {match, MatchModuleList} ->
                    MatchModuleList1 = lists:usort(MatchModuleList),
                    ModuleList1 =
                        case lists:any(fun([MatchModule]) ->
                            MatchModuleAtom = binary_to_atom(MatchModule),
                            case lists:keyfind(MatchModuleAtom, 1, CompileList) of
                                {_, _, _} -> true;
                                _ -> false
                            end
                                       end, MatchModuleList1)
                        of
                            true ->
                                case Module of
                                    %% 测试依赖
                                    xlsx2erl_test_dep -> BaseName = "xlsx2erl_test_dep";
                                    _ -> #xlsx2erl_excel{excel_name = BaseName} = xlsx2erl_util:get_excel(Module)
                                end,
                                [{Module, BaseName} | ModuleList];
                            _ ->
                                ModuleList
                        end,
                    compile_make_depend(T, CompileList, ModuleList1);
                _ ->
                    compile_make_depend(T, CompileList, ModuleList)
            end
    end.

close_dets() ->
    lists:foreach(fun(Name) ->
        case is_atom(Name) andalso "xlsx2erl_" -- atom_to_list(Name) == [] of
            true -> dets:close(Name);
            _ -> skip
        end
                  end, dets:all()).

%% @private 获取所有sheet的数据, 拿到的是倒序的!!
-spec get_sheet_data([{RecordName :: atom(), RecordFieldList :: [atom()]}], file:filename()) -> error|[#xlsx2erl_sheet{}].
get_sheet_data(RecordDefList, XlsxFile) ->
    RowHandler =
        fun(SheetName, [Line, Key | Row], {NthListList, SheetList} = Acc) ->
            case string:split(SheetName, "-") of
                [_, RecordNameStr] ->
                    case catch list_to_atom(RecordNameStr) of
                        RecordName when is_atom(RecordName) -> ok;
                        _ -> RecordName = undefined
                    end;
                _ ->
                    RecordName = undefined
            end,
            RecordDef = lists:keyfind(RecordName, 1, RecordDefList),
            Acc1 =
                case RecordDef =/= false andalso Key of
                    ?XLSX2ERL_KEY_MASK ->
                        {_RecordName, RecordFieldList} = RecordDef,
                        %% 构造#{Key => ColumnIndex}
                        {_, KeyMap} =
                            lists:foldl(fun(Column, {ColumnIndex, M}) ->
                                case catch list_to_atom(Column) of
                                    ColumnAtom when is_atom(ColumnAtom) ->
                                        {ColumnIndex + 1, M#{ColumnAtom => ColumnIndex}};
                                    _ ->
                                        {ColumnIndex + 1, M}
                                end
                                        end, {1, #{}}, Row),
                        case get_sheet_data_nth(RecordFieldList, KeyMap, []) of
                            NthList when is_list(NthList) ->
                                {
                                    [{RecordName, NthList} | NthListList],
                                    [#xlsx2erl_sheet{excel_name = XlsxFile, sheet_name = SheetName, name = RecordName, row_list = []} | SheetList]
                                };
                            Error ->
                                io:format("bad key in ~ts~n~p~n", [SheetName, Error]),
                                throw(error)
                        end;
                    ?XLSX2ERL_DATA_MASK ->
                        case lists:keyfind(RecordName, 1, NthListList) of
                            false ->% 未初始化key
                                Acc;
                            {_, NthList} ->
                                #xlsx2erl_sheet{row_list = RowList} = Sheet = lists:keyfind(RecordName, #xlsx2erl_sheet.name, SheetList),
                                RecordValueList = get_sheet_data_value(NthList, Row),
                                Record = list_to_tuple([RecordName | RecordValueList]),
                                RowList1 = [#xlsx2erl_row{line = Line, record = Record} | RowList],
                                {NthListList, lists:keystore(RecordName, #xlsx2erl_sheet.name, SheetList, Sheet#xlsx2erl_sheet{row_list = RowList1})}
                        end;
                    _ ->
                        Acc
                end,
            {next_row, Acc1};
            (_SheetName, _, Acc) ->
                {next_row, Acc}
        end,
    case xlsx_reader:read(XlsxFile, {[], []}, RowHandler) of
        {error, Reason} ->
            io:format("read ~ts error~n~p~n", [XlsxFile, Reason]),
            error;
        {_, List} ->
            List
    end.

get_sheet_data_nth([], _, NthList) ->
    lists:reverse(NthList);
get_sheet_data_nth([RecordField | T], KeyMap, NthList) ->
    case maps:get(RecordField, KeyMap, undefined) of
        undefined ->
            {not_exist, RecordField};
        Nth ->
            get_sheet_data_nth(T, KeyMap, [Nth | NthList])
    end.

get_sheet_data_value([], _) ->
    [];
get_sheet_data_value([H | T], Row) ->
    [get_sheet_data_value_1(H, Row) | get_sheet_data_value(T, Row)].

get_sheet_data_value_1(_N, []) ->
    [];
get_sheet_data_value_1(N, [H | _]) when N =< 1 ->
    H;
get_sheet_data_value_1(N, [_ | T]) ->
    get_sheet_data_value_1(N - 1, T).

%% @equiv make_template(XlsxFileName, default)
make_template(XlsxFileName) ->
    make_template(XlsxFileName, default).
%% @private 生成模板erl
-spec make_template(file:filename_all(), file:filename()) -> any().
make_template(XlsxFileName, OutDir) ->
    BaseName = filename:basename(XlsxFileName),
    case string:split(filename:rootname(BaseName), "-") of
        [_, TagStr] ->
            ModuleStr = "xlsx2erl_" ++ TagStr,
            RecordDefList = make_template_record_def(XlsxFileName),
            Head =
                "-module(" ++ ModuleStr ++ ").\n\n"
            "-behaviour(xlsx2erl_callback).\n\n"
            "-include(\"xlsx2erl.hrl\").\n\n"
            "-define(DETS_DICT1(Name), {Name, dict}).\n\n",
            RecordMask =
                ?XLSX2ERL_RECORD_START_MASK1(ModuleStr) ++ "\n" ++
                ["-record(" ++ RecordName ++ ", {" ++ string:join(FieldList, ", ") ++ "}).\n"
                    || [RecordName | FieldList] <- RecordDefList] ++
                ?XLSX2ERL_RECORD_END_MASK1(ModuleStr) ++ "\n\n",
            ExportDefaultGet =
                "-export([" ++ string:join(["get_" ++ RecordName ++ "/0" || [RecordName | _] <- RecordDefList], ", ") ++ "]).\n\n",
            ExportPrivate =
                "-export([update_dets/1, compile/1, clean/1]).\n\n",
            UpdateDets =
                "update_dets(FileName) ->\n"
                "    RecordDef = [\n" ++
                string:join([
                        "        {" ++ RecordName ++ ", record_info(fields, " ++ RecordName ++ ")}"
                    || [RecordName | _] <- RecordDefList], ",\n") ++ "\n" ++
                "    ],\n"
                "    case xlsx2erl:get_sheet_data(RecordDef, FileName) of\n"
                "        SheetList when is_list(SheetList) ->\n"
                "            %% todo 可以生成自定义数据, 可以用来优化表交叉验证数据的效率\n"
                "            %% todo 默认生成record第一个字段为key的dict结构\n"
                "            {SheetList1, DictList} = update_dets_convert(SheetList, [], []),\n"
                "            Now = erlang:localtime(),\n"
                "            dets:insert(?DETS_XLSX2ERL1(?MODULE), #xlsx2erl_excel{name = ?MODULE, excel_name = filename:basename(FileName), sheet_list = SheetList1}),\n"
                "            dets:insert(?DETS_XLSX2ERL1(?MODULE), #xlsx2erl_dets{k = ?XLSX2ERL_EXCEL_UPDATE_TIME, v = Now}),\n"
                "            %% todo 保存自定义数据\n"
                "            lists:foreach(fun({K, V}) ->\n"
                "                dets:insert(?DETS_XLSX2ERL1(?MODULE), #xlsx2erl_dets{k = ?DETS_DICT1(K), v = V})\n"
                "                          end, DictList);\n"
                "        _ ->\n"
                "            error\n"
                "    end.\n\n"
                "update_dets_convert([], SheetList, DictList) ->\n"
                "    {SheetList, DictList};\n" ++
                string:join([
                        "update_dets_convert([#xlsx2erl_sheet{name = " ++ RecordName ++ ", row_list = RowList} = H | T], SheetList, DictList) ->\n"
                    "    {OldRowList, OldDict} = update_dets_convert_sheet_and_dict(" ++ RecordName ++ ", SheetList, DictList),\n"
                    "    {RowList1, Dict1} = update_dets_convert_record_" ++ RecordName ++ "(RowList, H, record_info(fields, " ++ RecordName ++ "), OldRowList, OldDict),\n"
                    "    SheetList1 = lists:keystore(" ++ RecordName ++ ", #xlsx2erl_sheet.name, SheetList, H#xlsx2erl_sheet{row_list = RowList1}),\n"
                    "    DictList1 = lists:keystore(" ++ RecordName ++ ", 1, DictList, {" ++ RecordName ++ ", Dict1}),\n"
                    "    update_dets_convert(T, SheetList1, DictList1)"
                    || [RecordName | _] <- RecordDefList], ";\n") ++
                ".\n\n"
                "update_dets_convert_sheet_and_dict(Name, SheetList, DictList) ->\n"
                "    case lists:keyfind(Name, #xlsx2erl_sheet.name, SheetList) of\n"
                "        false -> OldRowList = [];\n"
                "        #xlsx2erl_sheet{row_list = OldRowList} -> ok\n"
                "    end,\n"
                "    case lists:keyfind(Name, 1, DictList) of\n"
                "        false -> OldDict = dict:new();\n"
                "        {_, OldDict} -> ok\n"
                "    end,\n"
                "    {OldRowList, OldDict}.\n\n" ++
                ["update_dets_convert_record_" ++ RecordName ++ "([], _Sheet, _RecordDef, RowList, Dict) ->\n"
                "    {RowList, Dict};\n"
                "update_dets_convert_record_" ++ RecordName ++ "([H | T], Sheet, RecordDef, RowList, Dict) ->\n"
                "    %% todo 选择转换类型\n"
                "    Record1 = #" ++ RecordName ++ "{\n" ++
                    string:join([
                            "        " ++ Field ++ " = xlsx2erl_util:convert_bin(#" ++ RecordName ++ "." ++ Field ++ ", RecordDef, H, Sheet)"
                        || Field <- FieldList], ",\n") ++ "\n"
                "    },\n"
                "    RowList1 = [H#xlsx2erl_row{record = Record1} | RowList],\n"
                "    %% todo 构造数据索引"
                "    Dict1 = dict:store(element(2, Record1), Record1, Dict),\n"
                "    update_dets_convert_record_" ++ RecordName ++ "(T, Sheet, RecordDef, RowList1, Dict1).\n\n"
                    || [RecordName | FieldList] <- RecordDefList],
            Get =
                "%% todo 对应的获取自定义数据方法, 默认获取dict\n" ++
                ["get_" ++ RecordName ++ "() ->\n"
                "    xlsx2erl_util:ensure_dets(?DETS_XLSX2ERL1(?MODULE)),\n"
                "    [#xlsx2erl_dets{v = V}] = dets:lookup(?DETS_XLSX2ERL1(?MODULE), ?DETS_DICT1(" ++ RecordName ++ ")),\n"
                "    V.\n\n"
                    || [RecordName | _] <- RecordDefList],
            Compile =
                "compile(#xlsx2erl_callback_args{export_path = ExportPath} = Args) ->\n"
                "    #xlsx2erl_excel{sheet_list = SheetList} = xlsx2erl_util:get_excel(?MODULE),\n"
                "    xlsx2erl_util:copy_mask_body(?MODULE, ExportPath ++ \"/\" ++ ?XLSX2ERL_DEFAULT_HRL),\n"
                "    do_compile(SheetList, Args).\n\n"
                "do_compile([], _Args) ->\n"
                "    ok;\n" ++
                string:join([
                        "do_compile([#xlsx2erl_sheet{name = " ++ RecordName ++ ", row_list = RowList} | T], #xlsx2erl_callback_args{export_path = ExportPath} = Args) ->\n"
                    "    Head =\n"
                    "        \"-module(\" ++ ?XLSX2ERL_DEFAULT_DATA_MODULE(" ++ RecordName ++ ") ++ \").\\n\\n\"\n"
                    "    \"-include(\\\"\" ++ ?XLSX2ERL_DEFAULT_HRL ++ \"\\\").\\n\\n\"\n"
                    "    \"-export([get/1]).\\n\\n\",\n"
                    "    Body =\n"
                    "        lists:map(fun(#xlsx2erl_row{record = Record}) ->\n"
                    "            %% todo 添加数值检查\n"
                    "            \"get(\" ++ xlsx2erl_util:to_iolist(Record#" ++ RecordName ++ "." ++ hd(FieldList) ++ ") ++ \") -> \\n\" ++\n"
                    "                \"    #" ++ RecordName ++ "{\"\n" ++
                        string:join([
                                "                \"" ++ Field ++ " = \" ++ xlsx2erl_util:to_iolist(Record#" ++ RecordName ++ "." ++ Field ++ ") ++"
                            || Field <- FieldList], " \", \" ++\n") ++ "\n"
                    "                \"};\\n\"\n"
                    "                  end, RowList),\n"
                    "    Tail =\n"
                    "        \"get(_) -> undefined.\",\n"
                    "    ok = file:write_file(ExportPath ++ \"/data_" ++ RecordName ++ ".erl\", [Head, Body, Tail]),\n"
                    "    do_compile(T, Args)"
                    || [RecordName | FieldList] <- RecordDefList], ";\n") ++ ".\n\n",
            Clean =
                "clean(#xlsx2erl_callback_args{export_path = ExportPath}) ->\n"
                "    file:delete(ExportPath ++ \"/\" ++ ?XLSX2ERL_DEFAULT_HRL),\n"
                "    catch dets:close(?DETS_XLSX2ERL1(?MODULE)),\n"
                "    file:delete(?DETS_PATH ++ \"/\" ++ ?MODULE_STRING),\n" ++
                string:join([
                        "    file:delete(ExportPath ++ \"/data_" ++ RecordName ++ ".erl\")"
                    || [RecordName | _] <- RecordDefList], ",\n") ++
                ".\n\n",
            ErlFile =
                case OutDir of
                    default ->
                        ?XLSX2ERL_CALLBACK_PATH ++ "/" ++ ModuleStr ++ ".erl";
                    _ ->
                        case filelib:is_dir(OutDir) of
                            true ->
                                OutDir ++ "/" ++ ModuleStr ++ ".erl";
                            _ ->
                                io:format("~ts not dir!", [OutDir]),
                                throw(badarg)
                        end
                end,
            io:format("write template file " ++ ErlFile ++ "~n"),
            ok = file:write_file(ErlFile, unicode:characters_to_binary([Head, RecordMask, ExportDefaultGet, ExportPrivate, UpdateDets, Get, Compile, Clean]));
        _ ->
            io:format("make template bad xlsx name ~ts~n", [BaseName])
    end.

make_template_record_def(XlsxFile) ->
    RowHandler =
        fun(SheetName, [_Line, ?XLSX2ERL_KEY_MASK | Row], Acc) ->
            case string:split(SheetName, "-") of
                [_, RecordNameStr] ->
                    FieldList =
                        lists:foldr(fun(Field, Acc1) ->
                            case (catch list_to_atom(Field)) of
                                FieldAtom when Field =/= [], is_atom(FieldAtom) ->
                                    [Field | Acc1];
                                _ ->
                                    Acc1
                            end
                                    end, [], Row),
                    {next_sheet, [[RecordNameStr | FieldList] | Acc]};
                _ ->
                    {next_sheet, Acc}
            end;
            (_, _, Acc) ->
                {next_row, Acc}
        end,
    case xlsx_reader:read(XlsxFile, [], RowHandler) of
        {error, Reason} ->
            io:format("get_sheet_name_list ~ts error~n~p~n", [filename:basename(XlsxFile), Reason]),
            error;
        RecordDefList ->
            %% 去重+翻转
            lists:foldl(fun([H | _] = RD, Acc) ->
                case lists:any(fun([ExistH | _]) -> ExistH == H end, Acc) of
                    true -> Acc;
                    _ -> [RD | Acc]
                end
                        end, [], RecordDefList)
    end.