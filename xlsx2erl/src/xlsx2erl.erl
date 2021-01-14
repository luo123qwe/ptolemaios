-module(xlsx2erl).

-include("xlsx2erl.hrl").
-include_lib("kernel/include/file.hrl").


%% private
-export([main/1, sheet_data_to_record/3]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    %% copy form log.erl
    Path = "./out.txt",
    file:delete(Path),
    Level = notice,
    ok = logger:add_handler(ptolemaios, logger_disk_log_h, #{config => #{file => Path,
        type => halt},
        level => Level,
        filesync_repeat_interval => 1000}),
    logger:update_formatter_config(ptolemaios, #{template => [level, " ", time, " ", pid, " ", mfa, ":", line, "\n", msg, "\n"], single_line => false}),
    logger:update_formatter_config(default, #{template => [level, " ", time, " ", pid, " ", mfa, ":", line, "\n", msg, "\n"], single_line => false}),
    logger:set_primary_config(#{level => Level}),
    case catch do_main(Args) of
        {'EXIT', ?XLSX2ERL_ERROR} -> ok;
        {'EXIT', Error} -> ?LOG_ERROR("unknow error ~w", [Error]);
        _ -> ok
    end,
    logger_disk_log_h:filesync(ptolemaios).

%% @private escript 自动执行export文件下所有的erl文件
do_main(["compile", XlsxDir0, ErlPath0, HrlPath0]) ->
    ErlPath = filename:absname(ErlPath0),
    HrlPath = filename:absname(HrlPath0),
    XlsxDir = filename:absname(XlsxDir0),
    file:make_dir(ErlPath),
    file:make_dir(HrlPath),
    file:del_dir_r(?XLSX2ERL_DETS_PATH),
    file:make_dir(?XLSX2ERL_DETS_PATH),
    
    WorkbookList = make_workbook_list(XlsxDir),
    UpdateDetsSheetList =
        lists:foldl(fun(#xlsx2erl_workbook{file_name = Filename}, Acc) ->
            xlsx2erl_reader:sheets_with_data(Filename) ++ Acc
                    end, [], WorkbookList),
    
    %% 基础设置
    erlang:process_flag(trap_exit, true),
    %% [{processor, [CoreInfo]}], Cpu数量
    CpuNum = length(element(2, hd(erlang:system_info(cpu_topology)))),
    %% 多进程更新dets
    compile_update_dets(UpdateDetsSheetList, [], CpuNum, CpuNum),
    
    %% 多进程调用对应compile函数
    UpdateModuleList = [Module || #xlsx2erl_sheet_with_data{module = Module} <- UpdateDetsSheetList],
    CallbackArgs = #xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath},
    compile_do_compile(UpdateModuleList, CallbackArgs, [], CpuNum, CpuNum),
    
    %% 更新md5
    update_md5(UpdateDetsSheetList),
    
    %% 关闭所有dets
    close_dets();
do_main(["compile_change", XlsxDir0, ErlPath0, HrlPath0]) ->
    ErlPath = filename:absname(ErlPath0),
    HrlPath = filename:absname(HrlPath0),
    XlsxDir = filename:absname(XlsxDir0),
    file:make_dir(ErlPath),
    file:make_dir(HrlPath),
    file:make_dir(?XLSX2ERL_DETS_PATH),
    
    %% 需要重新生成的xlsx和模块
    WorkbookList = make_workbook_list(XlsxDir),
    UpdateDetsSheetList =
        lists:foldl(fun(#xlsx2erl_workbook{name = WorkbookName, file_name = Filename}, Acc) ->
            xlsx2erl_util:ensure_dets(?XLSX2ERL_DETS_TABLE1(WorkbookName)),
            case dets:lookup(?XLSX2ERL_DETS_TABLE1(WorkbookName), ?XLSX2ERL_DETS_KEY_MD5) of
                [{_, OldMd5}] ->
                    {ok, Bin} = file:read_file(Filename),
                    case OldMd5 =/= erlang:md5(Bin) of
                        true ->% xlsx变了
                            xlsx2erl_reader:sheets_with_data_md5(Filename) ++ Acc;
                        _ ->
                            % 保证文件夹存在
                            ErlDir = ?XLSX2ERL_CB_PATH ++ "/" ++ atom_to_list(WorkbookName),
                            file:make_dir(ErlDir),
                            {ok, #file_info{mtime = ErlDirMTime}} = file:read_file_info(ErlDir),
                            %% 对比所有文件的修改时间
                            ModifyModuleList =
                                case dets:lookup(?XLSX2ERL_DETS_TABLE1(WorkbookName), ?XLSX2ERL_DETS_KEY_UPDATE) of
                                    [{_, UpdateTime}] when UpdateTime > ErlDirMTime ->
                                        [];
                                    [{_, UpdateTime}] ->
                                        filelib:fold_files(ErlDir, ".erl", false,
                                            fun(FN, ErlAcc) ->
                                                {ok, #file_info{mtime = ErlMTime}} = file:read_file_info(FN),
                                                case UpdateTime < ErlMTime of
                                                    true -> [list_to_atom(filename:basename(FN, ".erl")) | ErlAcc];
                                                    _ -> ErlAcc
                                                end
                                            end, []);
                                    _ ->
                                        filelib:fold_files(ErlDir, ".erl", false,
                                            fun(FN, ErlAcc) ->
                                                [list_to_atom(filename:basename(FN, ".erl")) | ErlAcc]
                                            end, [])
                                end,
                            case ModifyModuleList of
                                [] -> Acc;
                                _ ->
                                    [SheetWithData || SheetWithData <- xlsx2erl_reader:sheets_with_data(Filename),
                                        lists:member(SheetWithData#xlsx2erl_sheet_with_data.module, ModifyModuleList)
                                    ] ++ Acc
                            end
                    end;
                _ ->
                    xlsx2erl_reader:sheets_with_data(Filename) ++ Acc
            end
                    end, [], WorkbookList),
    
    %% 基础设置
    erlang:process_flag(trap_exit, true),
    %% [{processor, [CoreInfo]}], Cpu数量
    CpuNum = length(element(2, hd(erlang:system_info(cpu_topology)))),
    %% 多进程更新dets
    compile_update_dets(UpdateDetsSheetList, [], CpuNum, CpuNum),
    
    %% 构造依赖关系, 哪些模块(excel表)需要重新生成
    UpdateModuleList = [Module || #xlsx2erl_sheet_with_data{module = Module} <- UpdateDetsSheetList],
    {UpdateDependModuleList, _} = filelib:fold_files(?XLSX2ERL_CB_PATH, ".erl", true, fun compile_make_depend/2, {[], UpdateModuleList}),
    
    %% 多进程调用对应compile函数
    CallbackArgs = #xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath},
    compile_do_compile(UpdateModuleList ++ UpdateDependModuleList, CallbackArgs, [], CpuNum, CpuNum),
    
    %% 更新md5
    update_md5(UpdateDetsSheetList),
    
    %% 关闭所有dets
    close_dets();
do_main(["clean", ErlPath0, HrlPath0]) ->
    ErlPath = filename:absname(ErlPath0),
    HrlPath = filename:absname(HrlPath0),
    file:make_dir(ErlPath),
    file:make_dir(HrlPath),
    
    %% 执行所有callback
    Args = #xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath},
    filelib:fold_files(?XLSX2ERL_CB_PATH, ".erl", true,
        fun(FileName, _) ->
            Start = erlang:system_time(millisecond),
            Module = list_to_atom(filename:basename(FileName, ".erl")),
            case catch Module:clean(Args) of
                {'EXIT', Error} ->
                    ?XLSX2ERL_ERROR2("clean ~w error~n~p", [Module, Error]);
                _ ->
                    End = erlang:system_time(millisecond),
                    ?LOG_NOTICE("~w clean success, cost ~p ms", [Module, End - Start])
            end
        end, []),
    close_dets();
do_main(["template", XlsxFile, SheetName]) ->
    xlsx2erl_tmplate:make(XlsxFile, SheetName);
do_main(["template", XlsxFile, SheetName, ErlPath0, HrlPath0]) ->
    ErlPath = filename:absname(ErlPath0),
    HrlPath = filename:absname(HrlPath0),
    file:make_dir(ErlPath),
    file:make_dir(HrlPath),
    xlsx2erl_tmplate:make(XlsxFile, SheetName, ErlPath, HrlPath);
do_main(["template_hrl", XlsxFile, SheetName]) ->
    xlsx2erl_tmplate:make_hrl(XlsxFile, SheetName);
do_main(["template_hrl", XlsxFile, SheetName, HrlPath0]) ->
    HrlPath = filename:absname(HrlPath0),
    file:make_dir(HrlPath),
    xlsx2erl_tmplate:make_hrl(XlsxFile, SheetName, HrlPath);
do_main(_) ->
    io:format(
        "compile: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" compile xlsx export export/include\n"
        "compile_change: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" compile_change xlsx export export/include\n"
        "clean: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" clean xlsx export export/include\n"
        "template: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" template xlsx/T测试_test.xlsx goods\n"
        "template_hrl: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" template_hrl xlsx/T测试_test.xlsx goods\n").

%% 多进程编译
compile_update_dets([], [], _, _) ->
    ok;
compile_update_dets(CompileList, ReceiveList, SpawnNum, ProcessNum) when CompileList == [] orelse SpawnNum == 0 ->
    compile_receive_spawn(?FUNCTION_NAME, ReceiveList),
    compile_update_dets(CompileList, [], ProcessNum, ProcessNum);
compile_update_dets([#xlsx2erl_sheet_with_data{
    sheet = #xlsx2erl_sheet{workbook_name = WorkbookName, module = Module} = Sheet, data = Data
} | T], ReceiveList, SpawnNum, ProcessNum) ->
    %% dets开一下
    xlsx2erl_util:ensure_dets(?XLSX2ERL_DETS_TABLE1(WorkbookName)),
    Start = erlang:system_time(millisecond),
    Pid = spawn_link(fun() ->
        case (catch Module:update_dets(Sheet, Data)) of
            {'EXIT', {undef, [{Module, update_dets, [_, _], _} | _]}} ->
                %% 没有这个erlang文件, 自动创建模板
                xlsx2erl_tmplate:make(Sheet, Data),
                ?LOG_NOTICE("make_template ~p", [Module]),
                close_dets(),
                exit(?XLSX2ERL_ERROR);
            {'EXIT', Error} ->
                ?DO_IF_NOT(Error == ?XLSX2ERL_ERROR, ?LOG_ERROR("update_dets ~p error~n~p", [Module, Error])),
                close_dets(),
                file:delete(?XLSX2ERL_DETS_PATH ++ "/" ++ atom_to_list(Module)),
                exit(?XLSX2ERL_ERROR);
            _ ->
                End = erlang:system_time(millisecond),
                ?LOG_NOTICE("update dets ~ts, cost ~p ms", [Module, End - Start])
        end
                     end),
    ReceiveList1 = [{Pid, Module} | ReceiveList],
    compile_update_dets(T, ReceiveList1, SpawnNum - 1, ProcessNum).

%% 多进程编译
compile_do_compile([], _, [], _, _) ->
    ok;
compile_do_compile(CompileList, CallbackArgs, ReceiveList, SpawnNum, ProcessNum) when CompileList == [] orelse SpawnNum == 0 ->
    compile_receive_spawn(?FUNCTION_NAME, ReceiveList),
    compile_do_compile(CompileList, CallbackArgs, [], ProcessNum, ProcessNum);
compile_do_compile([Module | T], CallbackArgs, ReceiveList, SpawnNum, ProcessNum) ->
    Pid = spawn_link(fun() ->
        Start = erlang:system_time(millisecond),
        case catch Module:compile(CallbackArgs) of
            {'EXIT', Error} ->
                ?XLSX2ERL_ERROR2("compile ~w error~n~p", [Module, Error]);
            _ ->
                End = erlang:system_time(millisecond),
                ?LOG_NOTICE("compile ~w success, cost ~p ms", [Module, End - Start])
        end
                     end),
    compile_do_compile(T, CallbackArgs, [{Pid, Module} | ReceiveList], SpawnNum - 1, ProcessNum).

compile_receive_spawn(FunctionName, ReceiveList) ->
    ErrorList =
        lists:foldl(fun(_, Acc) ->
            receive
                {'EXIT', Pid, Reason} ->
                    case lists:keymember(Pid, 1, ReceiveList) andalso Reason == normal of
                        true ->
                            Acc;
                        _ ->
                            case lists:keyfind(Pid, 1, ReceiveList) of
                                false -> Module = undefined;
                                {_, Module} -> ok
                            end,
                            [{Module, Reason} | Acc]
                    end;
                Unknown ->
                    [{unknown, Unknown} | Acc]
            end
                    end, [], ReceiveList),
    case ErrorList of
        [] ->
            ok;
        _ ->
            close_dets(),
            case lists:filter(fun({_Module, Reason}) ->
                case Reason of
                    ?XLSX2ERL_ERROR -> false;
                    _ -> true
                end end, ErrorList)
            of
                [] -> exit(?XLSX2ERL_ERROR);
                ErrorList1 -> ?XLSX2ERL_ERROR2("compile error ~w: ~w", [FunctionName, ErrorList1])
            end
    end.

%% 定义:'依赖'总是通过 A表的Key==B表的Key 实现, 类似mysql的外键
%% 如果A文件被B文件依赖, B文件被C文件依赖(物品被装备依赖, 装备被装备属性依赖)
%% 当A文件改变时(物品), 同时重新生成被'直接'依赖的文件(装备), 但不会生成'间接'依赖的文件(装备属性)
%% 例如, A表数据变了, B/C表不变
%% 那么A表的key可能变了, 导致B表的key对应不上, 所以需要重新生成
%% 但是C表的key依赖B表的key, B表并没有改变, 所以不需要生成
compile_make_depend(ErlFileName, {NewModuleList, ModuleList}) ->
    ErlFileBaseName = filename:basename(ErlFileName, ".erl"),
    Module = list_to_atom(ErlFileBaseName),
    case lists:member(Module, ModuleList) of
        true ->
            {NewModuleList, ModuleList};
        _ ->
            {ok, Binary} = file:read_file(ErlFileName),
            %% 正则找出所有 非注释内的 xlsx2erl_xxx
            case re:run(Binary, "^([^%]*xlsx2erl_(?!util)[A-z0-9]+)", [global, multiline, {capture, [0], binary}]) of
                {match, MatchLineList} ->
                    %% 有没有用到 ModuleList 里面的模块
                    IsDepend =
                        lists:any(fun([MatchLine]) ->
                            {match, MatchModuleList} = re:run(MatchLine, "(xlsx2erl_(?!util)[A-z0-9]+)", [global, {capture, [0], binary}]),
                            lists:any(fun([MatchModule]) ->
                                MatchModuleAtom = binary_to_atom(MatchModule),
                                lists:member(MatchModuleAtom, ModuleList)
                                      end, MatchModuleList)
                                  end, MatchLineList),
                    case IsDepend of
                        true ->
                            %% 拿到module的工作簿, ?XLSX2ERL_DEFAULT_MODULE2
                            [_, WorkbookName | SheetName] = string:split(ErlFileBaseName, "_"),
                            WorkbookList = make_workbook_list(Module),
                            case lists:keyfind(list_to_atom(WorkbookName), #xlsx2erl_workbook.name, WorkbookList) of
                                false ->% xlsx被删了但是erl文件还在
                                    case Module of
                                        xlsx2erl_test_dep ->% 测试数据
                                            {[Module | NewModuleList], ModuleList};
                                        _ ->
                                            ?XLSX2ERL_ERROR2("xlsx not exist, but ~s use it!", [ErlFileName])
                                    end;
                                _ ->
                                    %% 检查数据还在不在
                                    xlsx2erl_util:ensure_dets(?XLSX2ERL_DETS_TABLE1(WorkbookName)),
                                    SheetNameAtom = list_to_atom(SheetName),
                                    case dets:member(?XLSX2ERL_DETS_TABLE1(WorkbookName), ?XLSX2ERL_DETS_KEY_SHEET1(SheetNameAtom)) of
                                        true ->
                                            {[Module | NewModuleList], ModuleList};
                                        _ ->
                                            % 代表这个模块对应的sheet没了
                                            ?XLSX2ERL_ERROR2("sheet ~s not exist, but ~s use it!", [ErlFileName])
                                    end
                            end;
                        _ ->
                            {NewModuleList, ModuleList}
                    end;
                _ ->
                    {NewModuleList, ModuleList}
            end
    end.

close_dets() ->
    lists:foreach(fun(Name) ->
        case is_atom(Name) andalso "xlsx_" -- atom_to_list(Name) == [] of
            true -> dets:close(Name);
            _ -> skip
        end
                  end, dets:all()).

make_workbook_list(XlsxDir) ->
    case get(?PD_XLSX2ERL_WORKBOOK_LIST) of
        undefined ->
            %% 搜索全部xlsx文件
            filelib:fold_files(XlsxDir, ".xlsx", true,
                fun(FileName, Acc) ->
                    BaseName = filename:basename(FileName),
                    FileNameFirstChar = hd(BaseName),
                    case FileNameFirstChar =/= $~
                        andalso string:split(filename:rootname(BaseName), ?XLSX2ERL_SPLIT)
                    of
                        [_, WorkbookName] ->
                            case catch list_to_atom(WorkbookName) of
                                WorkbookNameAtom when is_atom(WorkbookNameAtom), WorkbookNameAtom =/= '' ->
                                    [#xlsx2erl_workbook{name = WorkbookNameAtom, file_name = FileName} | Acc];
                                _ ->
                                    Acc
                            end;
                        false ->% 中间文件
                            Acc;
                        _ ->
                            Acc
                    end
                end, []);
        WorkbookList ->
            WorkbookList
    end.

%% 把行数据转换成record
%% xlsx2erl_raw_row 转换成 xlsx2erl_row
sheet_data_to_record(RawData, RecordName, FieldList) ->
    #{
        ?XLSX2ERL_KEY_KEY := [#xlsx2erl_raw_row{data = KeyRow} | _],
        ?XLSX2ERL_KEY_DATA := Data
    } = RawData,
    KeyRow1 = [{ColumnIndex, list_to_atom(Value)} || {ColumnIndex, Value} <- KeyRow, Value =/= ""],
    %% 先检查字段对不对得上
    ColumnIndexList =
        lists:map(fun(Field) ->
            case lists:keyfind(Field, 2, KeyRow1) of
                {ColumnIndex, _Value} ->
                    ColumnIndex;
                _ ->
                    ?XLSX2ERL_ERROR2("bad field ~w in record ~w~nvalid field list: ~w",
                        [Field, RecordName, [Key || {_, Key} <- []]])
            end
                  end, FieldList),
    lists:reverse(lists:foldl(fun(#xlsx2erl_raw_row{line = Line, data = DataRow}, Acc) ->
        FieldValueList = [element(2, lists:keyfind(ColumnIndex, 1, DataRow)) || ColumnIndex <- ColumnIndexList],
        Record = list_to_tuple([RecordName | FieldValueList]),
        [#xlsx2erl_row{line = Line, record = Record} | Acc]
                              end, [], Data)).


update_md5(UpdateDetsSheetList) ->
    ST = erlang:system_time(millisecond),
    lists:foldl(fun(#xlsx2erl_sheet_with_data{sheet = #xlsx2erl_sheet{workbook_name = WorkbookName, filename = Filename}}, Acc) ->
        case lists:member(WorkbookName, Acc) of
            true -> Acc;
            _ ->
                xlsx2erl_util:ensure_dets(?XLSX2ERL_DETS_TABLE1(WorkbookName)),
                {ok, Bin} = file:read_file(Filename),
                Md5 = erlang:md5(Bin),
                dets:insert(?XLSX2ERL_DETS_TABLE1(WorkbookName), {?XLSX2ERL_DETS_KEY_MD5, Md5}),
                %% erl文件更新用时间做粗比较, 全部文件md5一遍还是算了
                dets:insert(?XLSX2ERL_DETS_TABLE1(WorkbookName), {?XLSX2ERL_DETS_KEY_UPDATE, erlang:localtime()}),
                [WorkbookName | Acc]
        end
                end, [], UpdateDetsSheetList),
    ET = erlang:system_time(millisecond),
    ?LOG_NOTICE("update md5 cost ~w", [ET - ST]).
    
    
    