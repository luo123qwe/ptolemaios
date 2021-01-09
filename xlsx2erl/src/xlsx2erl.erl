-module(xlsx2erl).

-include("xlsx2erl.hrl").
-include_lib("kernel/include/file.hrl").
-include("util.hrl").

-define(RECORD_NAME1(Name), "data_" ++ Name).
%% make_sheet_list参数
-record(msl_arg, {
    filename,
    workbook_name,
    workbook_name_atom,
    last_sheet_full_name,
    sheet_name_atom,
    sheet_list = []
}).

%% private
-export([main/1, make_template/1, make_template/4, make_sheet/4, make_sheet_list/1, sheet_data_to_record/3]).

%%====================================================================
%% API functions
%%====================================================================

%% @private escript 自动执行export文件下所有的erl文件
main(["compile", XlsxDir0, ErlPath0, HrlPath0]) ->
    ErlPath = filename:absname(ErlPath0),
    HrlPath = filename:absname(HrlPath0),
    XlsxDir = filename:absname(XlsxDir0),
    file:make_dir(ErlPath),
    file:make_dir(HrlPath),
    file:make_dir(?XLSX2ERL_DETS_PATH),
    
    WorkbookList = make_workbook_list(XlsxDir),
    UpdateDetsSheetList =
        lists:foldl(fun(#xlsx2erl_workbook{file_name = Filename}, Acc) ->
            make_sheet_list(Filename) ++ Acc
                    end, [], WorkbookList),
    
    %% 基础设置
    erlang:process_flag(trap_exit, true),
    %% [{processor, [CoreInfo]}], Cpu数量
    CpuNum = length(element(2, hd(erlang:system_info(cpu_topology)))),
    %% 多进程更新dets
    compile_update_dets(UpdateDetsSheetList, [], CpuNum, CpuNum),
    
    %% 多进程调用对应compile函数
    CallbackArgs = #xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath},
    catch compile_do_compile(UpdateDetsSheetList, CallbackArgs, [], CpuNum, CpuNum),
    %% 关闭所有dets
    close_dets();
main(["compile_change", XlsxDir0, ErlPath0, HrlPath0]) ->
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
            case dets:match(?XLSX2ERL_DETS_TABLE1(WorkbookName), [{{?XLSX2ERL_DETS_KEY_UPDATE1('_'), '$1'}, [], ['$1']}]) of
                [_ | _] = UpdateTimeList ->
                    UpdateTime = lists:max(UpdateTimeList),
                    {ok, #file_info{mtime = MTime}} = file:read_file_info(Filename),
                    case UpdateTime < MTime of
                        true ->% xlsx变了
                            make_sheet_list(Filename) ++ Acc;
                        _ ->
                            % 保证文件夹存在
                            ErlDir = ?XLSX2ERL_CB_PATH ++ "/" ++ atom_to_list(WorkbookName),
                            file:make_dir(ErlDir),
                            %% 对比所有文件的修改时间
                            IsModifyErl =
                                filelib:fold_files(ErlDir, ".erl", false,
                                    fun(_FN, true) -> true;
                                        (FN, ErlAcc) ->
                                            {ok, #file_info{mtime = ErlMTime}} = file:read_file_info(FN),
                                            case UpdateTime < ErlMTime of
                                                true -> true;
                                                _ -> ErlAcc
                                            end
                                    end, false),
                            case IsModifyErl of
                                true -> make_sheet_list(Filename) ++ Acc;% .erl变了
                                _ -> Acc
                            end
                    end;
                _ ->
                    make_sheet_list(Filename) ++ Acc
            end
                    end, [], WorkbookList),
    
    %% 基础设置
    erlang:process_flag(trap_exit, true),
    %% [{processor, [CoreInfo]}], Cpu数量
    CpuNum = length(element(2, hd(erlang:system_info(cpu_topology)))),
    %% 多进程更新dets
    compile_update_dets(UpdateDetsSheetList, [], CpuNum, CpuNum),
    
    %% 构造依赖关系, 哪些模块(excel表)需要重新生成
    UpdateDependSheetList = filelib:fold_files(?XLSX2ERL_CB_PATH, ".erl", true, fun compile_make_depend/2, UpdateDetsSheetList),
    
    %% 多进程调用对应compile函数
    CallbackArgs = #xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath},
    catch compile_do_compile(UpdateDependSheetList, CallbackArgs, [], CpuNum, CpuNum),
    %% 关闭所有dets
    close_dets();
main(["clean", ErlPath0, HrlPath0]) ->
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
                    io:format("clean ~w error~n~p~n~n", [Module, Error]);
                _ ->
                    End = erlang:system_time(millisecond),
                    io:format("~w clean success, cost ~p ms~n", [Module, End - Start])
            end
        end, []),
    close_dets();
main(_) ->
    io:format(
        "compile: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" compile xlsx export export/include\n"
        "compile_change: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" compile_change xlsx export export/include\n"
        "clean: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" clean xlsx export export/include\n"
        "template: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" template xlsx/T测试-test.xlsx . .\n"
        "template_hrl: \"../rebar3\" escriptize&\"_build/default/bin/xlsx2erl\" template_hrl xlsx/T测试-test.xlsx .\n").

%% 多进程编译
compile_update_dets([], [], _, _) ->
    ok;
compile_update_dets(CompileList, ReceiveList, SpawnNum, ProcessNum) when CompileList == [] orelse SpawnNum == 0 ->
    compile_receive_spawn(?FUNCTION_NAME, ReceiveList),
    compile_update_dets(CompileList, [], ProcessNum, ProcessNum);
compile_update_dets([#xlsx2erl_sheet{workbook_name = WorkbookName, module = Module} = Sheet | T], ReceiveList, SpawnNum, ProcessNum) ->
    %% dets开一下
    xlsx2erl_util:ensure_dets(?XLSX2ERL_DETS_TABLE1(WorkbookName)),
    Start = erlang:system_time(millisecond),
    Pid = spawn_link(fun() ->
        case (catch Module:update_dets(Sheet)) of
            {'EXIT', {undef, [{Module, update_dets, [_], _} | _]}} ->
                %% 没有这个erlang文件, 自动创建模板
                make_template(Sheet),
                io:format("make_template ~p~n", [Module]),
                close_dets(),
                exit(make_template);
            {'EXIT', Error} ->
                io:format("update_dets ~p error~n~p~n~n", [Module, Error]),
                close_dets(),
                file:delete(?XLSX2ERL_DETS_PATH ++ "/" ++ atom_to_list(Module)),
                exit(make_template);
            _ ->
                End = erlang:system_time(millisecond),
                io:format("update dets ~ts, cost ~p ms~n", [Module, End - Start])
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
compile_do_compile([#xlsx2erl_sheet{module = Module} | T], CallbackArgs, ReceiveList, SpawnNum, ProcessNum) ->
    Pid = spawn_link(fun() ->
        Start = erlang:system_time(millisecond),
        case catch Module:compile(CallbackArgs) of
            {'EXIT', Error} ->
                io:format("compile ~w error~n~p~n~n", [Module, Error]);
            _ ->
                End = erlang:system_time(millisecond),
                io:format("compile ~w success, cost ~p ms~n", [Module, End - Start])
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
            io:format("compile error ~w: ~w", [FunctionName, ErrorList]),
            throw(error)
    end.

%% 定义:'依赖'总是通过 A表的Key==B表的Key 实现, 类似mysql的外键
%% 如果A文件被B文件依赖, B文件被C文件依赖(物品被装备依赖, 装备被装备属性依赖)
%% 当A文件改变时(物品), 同时重新生成被'直接'依赖的文件(装备), 但不会生成'间接'依赖的文件(装备属性)
%% 例如, A表数据变了, B/C表不变
%% 那么A表的key可能变了, 导致B表的key对应不上, 所以需要重新生成
%% 但是C表的key依赖B表的key, B表并没有改变, 所以不需要生成
compile_make_depend(ErlFileName, CompileList) ->
    ErlFileBaseName = filename:basename(ErlFileName, ".erl"),
    Module = list_to_atom(ErlFileBaseName),
    case lists:keymember(Module, #xlsx2erl_sheet.module, CompileList) of
        true ->
            CompileList;
        _ ->
            {ok, Binary} = file:read_file(ErlFileName),
            %% 正则找出所有 xlsx2erl_xxx: 的调用
            case re:run(Binary, "(xlsx2erl_(?!util)[A-z0-9]*):", [global, {capture, all_but_first, binary}]) of
                {match, MatchModuleList} ->
                    MatchModuleList1 = lists:usort(MatchModuleList),
                    case lists:any(fun([MatchModule]) ->
                        MatchModuleAtom = binary_to_atom(MatchModule),
                        lists:keymember(MatchModuleAtom, #xlsx2erl_sheet.module, CompileList)
                                   end, MatchModuleList1)
                    of
                        true ->
                            %% 拿到module的工作簿, ?XLSX2ERL_DEFAULT_MODULE2
                            [_, WorkbookName | SheetName] = string:split(ErlFileBaseName, "_"),
                            WorkbookList = make_workbook_list(Module),
                            case lists:keyfind(list_to_atom(WorkbookName), #xlsx2erl_workbook.name, WorkbookList) of
                                false ->% xlsx被删了但是erl文件还在
                                    case Module of
                                        xlsx2erl_test_dep ->% 测试数据
                                            [#xlsx2erl_sheet{module = xlsx2erl_test_dep}] ++ CompileList;
                                        _ ->
                                            io:format("~s not exist, but ~s use it!~n", [WorkbookName, ErlFileName]),
                                            exit(error)
                                    end;
                                #xlsx2erl_workbook{file_name = WorkbookFileName} ->
                                    case lists:keymember(WorkbookFileName, #xlsx2erl_sheet.filename, CompileList) of
                                        false ->
                                            make_sheet_list(WorkbookFileName) ++ CompileList;
                                        _ ->% sheet已经加载但是里面没有这个模块, 代表这个模块对应的sheet没了
                                            io:format("~s not exist, but ~s use it!~n", [SheetName, ErlFileName]),
                                            exit(error)
                                    end
                            end;
                        _ ->
                            CompileList
                    end;
                _ ->
                    CompileList
            end
    end.

close_dets() ->
    lists:foreach(fun(Name) ->
        case is_atom(Name) andalso "xlsx2erl_" -- atom_to_list(Name) == [] of
            true -> dets:close(Name);
            _ -> skip
        end
                  end, dets:all()).

%% 多次重复打开同一个workbook不会有任何优化
%% 而且打开工作簿本身占用更多时间
%% 所以在一次read遍历把所有的信息读完
make_sheet_list(FileName) ->
    case get(?PD_XLSX2ERL_SHEET_LIST1(FileName)) of
        undefined ->
            [_, WorkbookName] = string:split(filename:rootname(filename:basename(FileName)), "-"),
            Arg = #msl_arg{filename = FileName, workbook_name = WorkbookName, workbook_name_atom = list_to_atom(WorkbookName)},
            case xlsx_reader:read(FileName, Arg, fun make_sheet_list/3) of
                {error, Reason} ->
                    io:format("read ~ts error~n~p~n", [FileName, Reason]),
                    exit(error);
                #msl_arg{sheet_list = SheetList} ->
                    %% 把顺序倒回来
                    SheetList1 =
                        lists:foldl(fun(#xlsx2erl_sheet{data = RowList} = Sheet, Acc) ->
                            [Sheet#xlsx2erl_sheet{data = lists:reverse(RowList)} | Acc]
                                    end, [], SheetList),
                    put(?PD_XLSX2ERL_SHEET_LIST1(FileName), SheetList1),
                    SheetList1
            end;
        SheetList ->
            SheetList
    end.

make_sheet_list(SheetFullName, [_Line, _Key | _RowT] = Row, Arg) ->
    #msl_arg{last_sheet_full_name = LastSheetFullName} = Arg,
    %% 先检查表名, 同一个表避免反复检查
    case LastSheetFullName == SheetFullName of
        true ->
            make_sheet_list_1(Row, Arg);
        _ ->
            case string:split(SheetFullName, "-") of
                [_, SheetName1] ->
                    case catch list_to_atom(SheetName1) of
                        SheetNameAtom when is_atom(SheetNameAtom) ->
                            Arg1 = Arg#msl_arg{last_sheet_full_name = SheetFullName, sheet_name_atom = SheetNameAtom},
                            make_sheet_list_1(Row, Arg1);
                        _ ->
                            {next_sheet, Arg#msl_arg{last_sheet_full_name = undefined}}
                    end;
                _ ->
                    {next_sheet, Arg#msl_arg{last_sheet_full_name = undefined}}
            end
    end;
%% 没有有效内容的行
make_sheet_list(_, _, Arg) ->
    {next_row, Arg}.

make_sheet_list_1([_Line, ?XLSX2ERL_KEY_KEY | RowT], Arg) ->
    #msl_arg{
        filename = FileName, workbook_name = WorkbookName,
        workbook_name_atom = WorkbookNameAtom,
        last_sheet_full_name = SheetFullName,
        sheet_name_atom = SheetNameAtom,
        sheet_list = SheetList
    } = Arg,
    
    %% 索引对应的key
    {_, Index2Key} =
        lists:foldl(fun(Column, {ColumnIndex, I2K}) ->
            case catch list_to_atom(Column) of
                ColumnAtom when is_atom(ColumnAtom), ColumnAtom =/= '' ->
                    {ColumnIndex + 1, [{ColumnIndex, ColumnAtom} | I2K]};
                _ ->
                    {ColumnIndex + 1, I2K}
            end
                    end, {1, []}, RowT);
make_sheet_list_1([Line, ?XLSX2ERL_KEY_DATA | RowT], Arg) ->
    #msl_arg{
        filename = FileName,
        last_sheet_full_name = SheetFullName,
        sheet_name_atom = SheetNameAtom,
        sheet_list = SheetList
    } = Arg,
    case lists:keyfind(SheetNameAtom, #xlsx2erl_sheet.name, SheetList) of
        false ->% 未初始化key
            io:format("key mask row before data ~ts~n", [SheetFullName]),
            throw(error);
        #xlsx2erl_sheet{data = RowList} = Sheet ->
            RowKV = index2key_value([], RowT, 1, FileName, SheetFullName, Line),
            Sheet1 = Sheet#xlsx2erl_sheet{data = [#xlsx2erl_row{line = Line, data = RowKV} | RowList]},
            SheetList1 = lists:keyreplace(SheetNameAtom, #xlsx2erl_sheet.name, SheetList, Sheet1),
            {next_row, Arg#msl_arg{sheet_list = SheetList1}}
    end;
make_sheet_list_1(_Row, Arg) ->
    {next_row, Arg}.

index2key_value([], _, _, _, _, _) ->
    [];
index2key_value([{_, Key} | _], [], _, FileName, SheetFullName, Line) ->
    io:format("empty value in ~ts:~ts, ~w:~w", [FileName, SheetFullName, Key, Line]);
index2key_value([{Index, Key} | I2KT], [RowH | RowT], Index, FileName, SheetFullName, Line) ->
    [{Key, RowH} | index2key_value(I2KT, RowT, Index + 1, FileName, SheetFullName, Line)];
index2key_value(I2KList, [_RowH | RowT], Index, FileName, SheetFullName, Line) ->
    index2key_value(I2KList, RowT, Index + 1, FileName, SheetFullName, Line).

make_workbook_list(XlsxDir) ->
    case get(?PD_XLSX2ERL_WORKBOOK_LIST) of
        undefined ->
            %% 搜索全部xlsx文件
            filelib:fold_files(XlsxDir, ".xlsx", true,
                fun(FileName, Acc) ->
                    BaseName = filename:basename(FileName),
                    FileNameFirstChar = hd(BaseName),
                    case FileNameFirstChar =/= $~
                        andalso string:split(filename:rootname(BaseName), "-")
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

sheet_data_to_record(#xlsx2erl_sheet{data = Data}, RecordName, FieldList) ->
    #{?XLSX2ERL_KEY_KEY := [#xlsx2erl_raw_row{data = KeyRow}], ?XLSX2ERL_KEY_DATA := Data} = Data,
    KeyRow1 = [{ColumnIndex, list_to_atom(Value)} || {ColumnIndex, Value} <- KeyRow, Value =/= ""],
    %% 先检查字段对不对得上
    ColumnIndexList =
        lists:map(fun(Field) ->
            case lists:keyfind(Field, 2, KeyRow1) of
                {ColumnIndex, _Value} ->
                    ColumnIndex;
                _ ->
                    io:format("bad field ~w in record ~w~nvalid field list: ~w~n",
                        [Field, RecordName, [Key || {_, Key} <- []]]),
                    exit(error)
            end
                  end, FieldList),
    case length(ColumnIndexList) > 16 of
        true ->
            lists:reverse(lists:foldl(fun(#xlsx2erl_raw_row{data = DataRow} = Row, Acc) ->
                FieldValueList =
                    lists:reverse(lists:foldl(fun(ColumnIndex, {FVL, DR}) ->
                        {value, {_, FieldValue}, DR1} = lists:keytake(ColumnIndex, 1, DR),
                        {[FieldValue | FVL], DR1}
                                              end, {[], DataRow}, ColumnIndexList)),
                Record = list_to_tuple([RecordName | FieldValueList]),
                [Row#xlsx2erl_row{data = Record} | Acc]
                                      end, [], Data));
        _ ->
            lists:reverse(lists:foldl(fun(#xlsx2erl_raw_row{data = DataRow} = Row, Acc) ->
                FieldValueList = [element(2, lists:keyfind(ColumnIndex, 1, DataRow)) || ColumnIndex <- ColumnIndexList],
                Record = list_to_tuple([RecordName | FieldValueList]),
                [Row#xlsx2erl_row{data = Record} | Acc]
                                      end, [], Data))
    end.

%% @private 获取sheet的数据, 转换成Record
-spec make_sheet(file:filename(), atom(), RecordName :: atom(), RecordFieldList :: [atom()]) -> error|[#xlsx2erl_sheet{}].
make_sheet(FileName, Module, RecordName, RecordFieldList) ->
    [_, WorkbookName] = string:split(filename:rootname(filename:basename(FileName)), "-"),
    RowHandler =
        fun(SheetFullName, [Line, Key | Row], {NL, S} = Acc) ->
            IsThisSheet =
                case string:split(SheetFullName, "-") of
                    [_, SheetName] ->
                        case catch ?XLSX2ERL_ERL_NAME2(WorkbookName, SheetName) of
                            Module -> true;
                            _ -> false
                        end;
                    _ ->
                        SheetName = "undefined",
                        false
                end,
            case IsThisSheet andalso Key of
                false ->
                    {next_sheet, Acc};
                ?XLSX2ERL_KEY_KEY when NL == undefined ->% 用一个找到的sheet的key
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
                            {next_row, {
                                NthList,
                                #xlsx2erl_sheet{filename = FileName, full_name = SheetFullName, name = list_to_atom(SheetName), data = []}
                            }};
                        Error ->
                            io:format("bad key in ~ts~n~p~n", [SheetFullName, Error]),
                            throw(error)
                    end;
                ?XLSX2ERL_KEY_DATA ->
                    case NL of
                        undefined ->% 未初始化key
                            io:format("key mask row before data ~ts~n", [SheetFullName]),
                            throw(error);
                        _ ->
                            #xlsx2erl_sheet{data = RowList} = S,
                            RecordValueList = get_sheet_data_value(NL, Row),
                            Record = list_to_tuple([RecordName | RecordValueList]),
                            RowList1 = [#xlsx2erl_row{line = Line, data = Record} | RowList],
                            {next_row, {NL, S#xlsx2erl_sheet{data = RowList1}}}
                    end;
                _ ->
                    {next_row, Acc}
            end;
            (_SheetName, _, Acc) ->
                {next_row, Acc}
        end,
    case xlsx_reader:read(FileName, {[], []}, RowHandler) of
        {error, Reason} ->
            io:format("read ~ts error~n~p~n", [FileName, Reason]),
            error;
        {_, Sheet} ->
            Sheet
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
make_template(#xlsx2erl_sheet{filename = XlsxFileName, name = SheetName}) ->
    make_template(XlsxFileName, atom_to_list(SheetName), default, default).
%% @private 生成模板erl
make_template(XlsxFileName, SheetName, ErlPath, HrlPath) ->
    case make_template_get_base_info(XlsxFileName, SheetName) of
        {ok, WorkbookName, RecordName, RecordComment, FieldCommentList} ->
            ModuleStr = ?XLSX2ERL_ERL_NAME2(WorkbookName, SheetName),
            Key = element(1, hd(RecordComment)),
            Head =
                "-module(" ++ ModuleStr ++ ").\n\n"
            "-behaviour(xlsx2erl_callback).\n\n"
            "-include(\"util.hrl\").\n"
            "-include(\"xlsx2erl.hrl\").\n"
            "-include(\"" ++ ModuleStr ++ ".hrl\").\n\n"
            "%% workbook和sheet的名字\n"
            "-define(PRIV_WORKBOOK_NAME, \"" ++ WorkbookName ++ "\").\n"
            "-define(PRIV_SHEET_NAME, \"" ++ SheetName ++ "\").\n"
            "%% dets\n"
            "-define(PRIV_DETS, ?DETS_TABLE1(?PRIV_WORKBOOK_NAME)).\n"
            "-define(PRIV_DETS_SHEET, list_to_atom(?PRIV_SHEET_NAME)).\n"
            "-define(PRIV_DETS_INDEX, {index, ?MODULE}).\n"
            "%% todo 生成的文件名字, hrl复制的mask\n"
            "-define(PRIV_ERL_FILE, " ++ RecordName ++ ".erl\").\n"
            "-define(PRIV_HRL_FILE, " ++ ?XLSX2ERL_HRL_NAME1(WorkbookName) ++ ".hrl).\n"
            "-define(PRIV_MASK_TAG, " ++ RecordName ++ ".hrl).\n"
            "",
            Export =
                "-export([get_index/0]).\n\n"
                "-export([update_dets/1, compile/1, clean/1]).\n\n",
            Pd =
                "%% 字典数据, 用于报错\n"
                "init_pd() ->\n"
                "    put(?PD_XLSX2ERL_WORKBOOK, ?PRIV_WORKBOOK_NAME),\n"
                "    put(?PD_XLSX2ERL_SHEET, ?PRIV_SHEET_NAME),\n"
                "    put(?PD_XLSX2ERL_FIELD_DEF, record_info(fields, " ++ RecordName ++ ")).\n\n"
            "clean_pd() ->\n"
            "    erase(?PD_XLSX2ERL_WORKBOOK),\n"
            "    erase(?PD_XLSX2ERL_SHEET),\n"
            "    erase(?PD_XLSX2ERL_FIELD_DEF),\n"
            "    erase(?PD_XLSX2ERL_ROW).\n\n"
            "",
            UpdateDets =
                "update_dets(Sheet) ->\n"
                "    init_pd(),\n"
                "    RowList = xlsx2erl:make_row_list(Sheet, " ++ RecordName ++ ", record_info(fields, " ++ RecordName ++ ")),\n"
            "    %% todo 可以生成自定义数据索引, 用来优化表交叉验证数据的效率\n"
            "    %% todo 默认生成record第一个字段为key的map结构\n"
            "    {RowList1, Index} = update_dets_convert_record(RowList, [], #{}),\n"
            "    Now = erlang:localtime(),\n"
            "    dets:insert(?PRIV_DETS, Sheet#xlsx2erl_sheet{row_list = RowList1}),\n"
            "    dets:insert(?PRIV_DETS, #xlsx2erl_dets{k = ?XLSX2ERL_EXCEL_UPDATE_TIME, v = Now}),\n"
            "    %% 保存自定义数据\n"
            "    dets:insert(?PRIV_DETS, #xlsx2erl_dets{k = ?PRIV_DETS_INDEX, v = Index}),\n"
            "\n"
            "    clean_pd().\n\n"
            "update_dets_convert_record([], RowList, Index) ->\n"
            "    {RowList, Index};\n"
            "update_dets_convert_record([H | T], RowList, Index) ->\n"
            "    put(?PD_XLSX2ERL_ROW, H),\n"
            "    %% todo 选择转换类型\n"
            "    Record1 = #" ++ RecordName ++ "{\n" ++
                string:join([
                        "        " ++ Field ++ " = ?XLSX2ERL_TO_(#" ++ RecordName ++ "." ++ Field ++ ", RecordDef, H, Sheet)"
                    || {Field, _} <- FieldCommentList], ",\n") ++ "\n"
            "    },\n"
            "    RowList1 = [H#xlsx2erl_row{record = Record1} | RowList],\n"
            "    %% todo 构造数据索引\n"
            "    Index1 = Index#{Record1#data_test_goods.id => Record1},\n"
            "    update_dets_convert_record(T, RowList1, Index1).\n\n"
            "",
            Index =
                "%% todo 对应的获取自定义索引\n" ++
                "get_index() ->\n"
                "    xlsx2erl_util:ensure_dets(?PRIV_DETS),\n"
                "    [#xlsx2erl_dets{v = V}] = dets:lookup(?PRIV_DETS, ?PRIV_DETS_INDEX),\n"
                "    V.\n\n",
            Compile =
                "compile(#xlsx2erl_cb_args{hrl_path = HrlPath, erl_path = ErlPath}) ->"
                "    [#xlsx2erl_sheet{row_list = RowList}] = dets:lookup(?PRIV_DETS, ?PRIV_DETS_SHEET),\n"
                "    %% todo 如果不需要复制hrl定义可以删除这句\n"
                "    xlsx2erl_util:copy_mask_body(?MODULE, ?PRIV_MASK_TAG, HrlPath ++ " / " ++ ?PRIV_HRL_FILE),\n"
                "\n"
                "    %% todo 构造文件内容, 如果生成多个函数, priv_arg多定义几个参数即可\n"
                "    Head =\n"
                "        \"-module(\" ++ filename:rootname(?PRIV_ERL_FILE) ++ \").\n\n\"\n"
                "    \"-include(\\\"\" ++ ?PRIV_HRL_FILE ++ \"\\\").\\n\\n\"\n"
                "    \"-export([get/1]).\\n\\n\",\n"
                "    #priv_arg{body = Body} = compile_row_list(RowList, #priv_arg{}),\n"
                "    Tail =\n"
                "        \"get(_) -> undefined.\",\n"
                "    file:make_dir(ErlPath ++ \"/\" ++ ?PRIV_WORKBOOK_NAME),\n"
                "    File = ErlPath ++ \"/\" ++ ?PRIV_WORKBOOK_NAME ++ \"/\" ++ ?PRIV_ERL_FILE\n"
                "    ok = file:write_file(File, [Head, Body, Tail]),\n"
                "\n"
                "    clean_pd()\n\n"
                "compile_row_list([], Arg) -> Arg;\n"
                "compile_row_list([H | T], Arg) ->\n"
                "    %% 再套一层, 提示报错\n"
                "    put(?PD_XLSX2ERL_ROW, H),\n"
                "    case catch compile_row(H, Arg) of\n"
                "        {'EXIT', ?XLSX2ERL_ERROR} ->% 已知错误\n"
                "            exit(?XLSX2ERL_ERROR);\n"
                "        #priv_arg{} = Arg1 ->\n"
                "            compile_row_list(T, Arg1);\n"
                "        Error ->\n"
                "            ?XLSX2ERL_PD_ERROR2(\"unknow error ~w~n\", [Error]),\n"
                "            exit(?XLSX2ERL_ERROR)\n"
                "    end.\n\n"
                "compile_row(#xlsx2erl_row{data = Record}, #priv_arg{body = Body, keys = Keys}) ->\n"
                "    %% todo 添加数值检查或转换\n"
                "    %% todo 是否重复key\n"
                "    Key = Record#" ++ RecordName ++ "." ++ Key ++ "\n"
            "    ?DO_IF(maps:is_key(Key, Keys), ?XLSX2ERL_PD_ERROR2(\"key ~w 重复了\", [Key])),\n"
            "    %% todo 添加数值转换\n"
            "    Record1 = Record#" ++ RecordName ++ "{}\n"
            "    BodyRow = compile_body(Record1),\n"
            "    Body1 = [BodyRow | Body],\n"
            "    Keys1 = Keys#{Key => 1},\n"
            "    {ok, #priv_arg{body = Body1, keys = Keys1}}.\n\n"
            "%% todo 构造文本\n"
            "compile_body(Record) ->\n"
            "    \"get(\" ++ xlsx2erl_util:to_iolist(Record#" ++ RecordName ++ "." ++ Key ++ ") ++ \") -> \\n\" ++\n"
            "        \"    #" ++ RecordName ++ "{\"\n" ++
                string:join([
                        "        \"        " ++ Field ++ " = \" ++ xlsx2erl_util:to_iolist(Record#" ++ RecordName ++ "." ++ Field ++ ") ++"
                    || {Field, _} <- RecordComment], " \", \" ++\n") ++ "\n"
            "        \"};\\n\".\n\n"
            "",
            Clean =
                "clean(#xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath}) ->"
                "        %% 先删dets\n"
                "    xlsx2erl_util:ensure_dets(?PRIV_DETS),\n"
                "    dets:delete(?PRIV_DETS, ?PRIV_DETS_SHEET),\n"
                "    dets:delete(?PRIV_DETS, ?PRIV_DETS_INDEX),\n"
                "    %% 再删文件\n"
                "    %% todo 如果不需要复制hrl定义可以删除这句\n"
                "    xlsx2erl_util:delete_mask_body(?PRIV_MASK_TAG, HrlPath ++ \"/\" ++ ?PRIV_HRL_FILE),\n"
                "    file:delete(ErlPath ++ \"/\" ++ ?PRIV_WORKBOOK_NAME ++ \"/\" ++ ?PRIV_ERL_FILE).\n"
                "",
            ErlFile =
                case ErlPath of
                    default ->
                        ?XLSX2ERL_CB_PATH ++ "/" ++ ModuleStr ++ ".erl";
                    _ ->
                        case filelib:is_dir(ErlPath) of
                            true ->
                                ErlPath ++ "/" ++ ModuleStr ++ ".erl";
                            _ ->
                                io:format("~ts not dir!", [ErlPath]),
                                throw(badarg)
                        end
                end,
            io:format("write template file " ++ ErlFile ++ "~n"),
            ok = file:write_file(ErlFile, unicode:characters_to_binary([Head, Export, UpdateDets, Compile, Index, Clean])),
            make_hrl_template(ModuleStr, FieldCommentList, HrlPath);
        Error ->
            Error
    end.

make_hrl_template(XlsxFileName, HrlPath) ->
%%    case make_template_get_base_info(XlsxFileName) of
%%        {ok, ModuleStr, KeyCommentList} ->
%%            make_hrl_template(ModuleStr, KeyCommentList, HrlPath);
%%        Error ->
%%            Error
%%    end.
    ok.

make_hrl_template(ModuleStr, KeyCommentList, HrlPath) ->
    RecordMask =
        [[?XLSX2ERL_MASK_START1(RecordName) ++ "\n"
        "%% " ++ RecordNameComment ++ "\n"
        "-record(", RecordName, ", {\n"
            , make_field_comment_str(FieldList),
                "}).\n" ++
                ?XLSX2ERL_MASK_END1(RecordName) ++ "\n"
        ] || [{RecordName, RecordNameComment} | FieldList] <- KeyCommentList],
    HrlFile =
        case HrlPath of
            default ->
                ?XLSX2ERL_INCLUDE_PATH ++ "/" ++ ModuleStr ++ ".hrl";
            _ ->
                case filelib:is_dir(HrlPath) of
                    true ->
                        HrlPath ++ "/" ++ ModuleStr ++ ".hrl";
                    _ ->
                        io:format("~ts not dir!", [HrlPath]),
                        throw(badarg)
                end
        end,
    io:format("write template file " ++ HrlFile ++ "~n"),
    ok = file:write_file(HrlFile, unicode:characters_to_binary([RecordMask])).

%% 获取一些基础信息用于生成模板
make_template_get_base_info(XlsxFileName, SheetName) ->
    BaseName = filename:basename(XlsxFileName),
    case string:split(filename:rootname(BaseName), "-") of
        [_, WorkbookName] ->
            {SheetFullName, MaskList} = make_template_get_mask_info(XlsxFileName, SheetName),
            KeyList =
                case lists:keyfind(?XLSX2ERL_KEY_KEY, 1, MaskList) of
                    false ->
                        io:format("no key in ~ts: ~s", [XlsxFileName, SheetName]),
                        exit(error);
                    {_, KeyList0} ->
                        KeyList0
                end,
            CommentList =
                case lists:keyfind(?XLSX2ERL_KEY_COMMENT, 1, MaskList) of
                    false ->
                        [];
                    {_, CommentList0} ->
                        CommentList0
                end,
            KeyCommentList = make_field_comment_tuple(KeyList, CommentList),
            RecordName = atom_to_list(?XLSX2ERL_RECORD_NAME2(WorkbookName, SheetName)),
            RecordComment = hd(string:split(SheetFullName, "-")),
            {ok, WorkbookName, RecordName, RecordComment, KeyCommentList};
        _ ->
            io:format("make template bad xlsx name ~ts~n", [BaseName]),
            error
    end.

%% 获取所有sheet的mask行对应的数据
%% [{sheet名, record名字, [字段]}]
make_template_get_mask_info(XlsxFile, SheetName) ->
    RowHandler =
        fun(SheetFullName, Row, {LastSheetFullName, RL, SL, SearchLine}) ->
            IsThisSheet =
                case LastSheetFullName == SheetFullName orelse string:split(SheetFullName, "-") of
                    true -> true;
                    [_, SheetName] ->
                        true;
                    _ ->
                        false
                end,
            case IsThisSheet of
                true ->
                    case Row of
                        [_, Mask | RowT] ->
                            make_template_get_mask_info_1(Mask, RowT, SheetFullName, RL, SL, SearchLine);
                        _ ->
                            {next_row, {SheetFullName, RL, SL, SearchLine}}
                    end;
                _ ->
                    {next_sheet, {undefined, RL, SL, ?XLSX2ERL_KEY_SEARCH_LIMIT}}
            end
        end,
    case xlsx_reader:read(XlsxFile, [], RowHandler) of
        {error, Reason} ->
            io:format("~ts get_mask_info error~n~p~n", [filename:basename(XlsxFile), Reason]),
            error;
        {MaskSheetFullName, ResultList, _SearchList, _} ->
            {MaskSheetFullName, ResultList}
    end.

make_template_get_mask_info_1(_, _, SheetFullName, ResultList, SearchList, 0) ->
    {break, {SheetFullName, ResultList, SearchList, 0}};
make_template_get_mask_info_1(Key, Row, SheetFullName, ResultList, SearchList, SearchLine) ->
    case lists:member(Key, SearchList) of
        true ->
            case lists:delete(Key, SearchList) of
                [] ->% 全部mask收集完
                    {break, {SheetFullName, [{Key, Row} | ResultList], [], SearchLine - 1}};
                SearchList1 ->
                    {next_row, {SheetFullName, [{Key, Row} | ResultList], SearchList1, SearchLine - 1}}
            end;
        _ ->
            {next_row, {SheetFullName, ResultList, SearchList, SearchLine - 1}}
    end.

%% [[{record_name, 注释}|[{field, 注释}]]]
make_field_comment_tuple([], _) ->
    [];
make_field_comment_tuple([{SheetNameHead, RecordName, FieldList} | T], AllCommentList) ->
    case lists:keyfind(RecordName, 2, AllCommentList) of
        false ->
            [[{RecordName, []} | [{Field, []} || Field <- FieldList, Field =/= []]]
                | make_field_comment_tuple(T, AllCommentList)];
        {_, _, CommentList} ->
            [[{RecordName, SheetNameHead} | make_field_comment_tuple1(FieldList, CommentList)]
                | make_field_comment_tuple(T, AllCommentList)]
    end.

make_field_comment_tuple1([], _) ->
    [];
make_field_comment_tuple1(L, []) ->% 后续的comment没有了
    [{Field, []} || Field <- L, Field =/= []];
make_field_comment_tuple1([[] | FT], [_ | CT]) ->% 空的cell
    make_field_comment_tuple1(FT, CT);
make_field_comment_tuple1([FH | FT], [CH | CT]) ->
    [{FH, CH} | make_field_comment_tuple1(FT, CT)].

make_field_comment_str([{F, C}]) ->
    ["    ", F, "% ", C, "\n"];
make_field_comment_str([{F, C} | T]) ->
    ["    ", F, ",% ", C, "\n" | make_field_comment_str(T)].
    
    
    
    
    
    
    
    