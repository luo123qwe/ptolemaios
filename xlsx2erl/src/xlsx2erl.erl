-module(xlsx2erl).

-include("xlsx2erl.hrl").
-include_lib("kernel/include/file.hrl").

%% API exports
-export([main/1, get_sheet_data/2]).

-export([copy_mask_body/2, copy_mask_body/4, get_excel/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript 自动执行export文件下所有的erl文件
main(["compile", XlsxDir0, ExportDir0]) ->
    ExportDir = filename:absname(ExportDir0),
    XlsxDir = filename:absname(XlsxDir0),
    file:make_dir(ExportDir),
    file:make_dir(?DETS_PATH),
    ModuleList =
        filelib:fold_files(XlsxDir, ".xlsx", true,
            fun(FileName, Acc) ->
                case string:split(filename:rootname(filename:basename(FileName)), "-") of
                    [_, TagStr] ->
                        ModuleStr = "xlsx2erl_" ++ TagStr,
                        Module = list_to_atom(ModuleStr),
                        {ok, ?DETS_XLSX2ERL1(Module)} = dets:open_file(?DETS_XLSX2ERL1(Module), [{file, ?DETS_PATH ++ "/" ++ ModuleStr}, {keypos, #excel.name}]),
                        NeedUpdateDets =
                            case dets:lookup(?DETS_XLSX2ERL1(Module), ?XLSX2ERL_DETS_EXCEL_UPDATE1(Module)) of
                                [?XLSX2ERL_DETS_EXCEL_UPDATE2(_, UpdateTime)] ->
                                    {ok, #file_info{mtime = MTime}} = file:read_file_info(FileName),
                                    UpdateTime < MTime;
                                _ ->
                                    true
                            end,
                        Acc1 =
                            case NeedUpdateDets andalso (catch Module:update_dets(FileName)) of
                                false when NeedUpdateDets == false ->
                                    Acc;
                                {'EXIT', {undef, [{Module, update_dets, [FileName], _} | _]}} ->
                                    %% 没有这个erlang文件, 自动创建模板
                                    make_template(FileName),
                                    io:format("make_template ~p~n", [Module]),
                                    dets:close(?DETS_XLSX2ERL1(Module)),
                                    throw(error);
                                {'EXIT', Error} ->
                                    io:format("update_dets ~p error~n~p~n~n", [Module, Error]),
                                    dets:close(?DETS_XLSX2ERL1(Module)),
                                    file:delete(?DETS_PATH ++ "/" ++ ModuleStr),
                                    throw(error);
                                _ ->
                                    io:format("update dets ~ts~n", [FileName]),
                                    [Module | Acc]
                            end,
                        Acc1;
                    _ ->
                        io:format("bad file name ~ts", [FileName]),
                        Acc
                end
            end, []),
    
    %% todo 构造依赖关系
    
    CallbackArgs = #callback_args{export_path = ExportDir},
    lists:foreach(fun(Module) ->
        case catch Module:compile(CallbackArgs) of
            {'EXIT', Error} ->
                io:format("compile ~p error~n~p~n~n", [Module, Error]);
            _ ->
                ok
        end
                  end, ModuleList);
main(["clean", XlsxDir0, ExportDir0]) ->
    ExportDir = filename:absname(ExportDir0),
    XlsxDir = filename:absname(XlsxDir0),
    file:make_dir(ExportDir),
    filelib:fold_files(XlsxDir, ".xlsx", true,
        fun(FileName, _) ->
            case string:split(filename:rootname(filename:basename(FileName)), "-") of
                [_, ModuleStr] ->
                    Module = list_to_atom("xlsx2erl_" ++ ModuleStr),
                    Args = #callback_args{
                        export_path = ExportDir
                    },
                    case catch Module:clean(Args) of
                        {'EXIT', Error} ->
                            case code:is_loaded(Module) of
                                false ->
                                    %% 没有这个erlang文件
                                    skip;
                                _ ->
                                    io:format("clean ~p error~n~p~n~n", [Module, Error])
                            end;
                        _ ->
                            ok
                    end;
                _ ->
                    io:format("bad file name ~ts", [FileName])
            end
        end, []).

%% @doc 获取excel
-spec get_excel(atom()) -> undefined|#excel{}.
get_excel(Module) ->
    case dets:info(?DETS_XLSX2ERL1(Module)) of
        undefined ->
            {ok, ?DETS_XLSX2ERL1(Module)} = dets:open_file(?DETS_XLSX2ERL1(Module), [{file, ?DETS_PATH ++ "/" ++ atom_to_list(Module)}, {keypos, #excel.name}]);
        _ -> ok
    end,
    case dets:lookup(?DETS_XLSX2ERL1(Module), Module) of
        [Excel] -> Excel;
        _ -> undefined
    end.

%% @doc 获取所有sheet的数据, 拿到的是倒序的!!
-spec get_sheet_data([{RecordName :: atom(), RecordFieldList :: [atom()]}], file:filename()) -> error|[#sheet{}].
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
                    "KEY" ->
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
                                    [#sheet{excel_name = XlsxFile, sheet_name = SheetName, name = RecordName, row_list = []} | SheetList]
                                };
                            Error ->
                                io:format("bad key in ~ts~n~p~n", [SheetName, Error]),
                                throw(error)
                        end;
                    "DATA" ->
                        case lists:keyfind(RecordName, 1, NthListList) of
                            false ->% 未初始化key
                                Acc;
                            {_, NthList} ->
                                #sheet{row_list = RowList} = Sheet = lists:keyfind(RecordName, #sheet.name, SheetList),
                                RecordValueList = get_sheet_data_value(NthList, Row),
                                Record = list_to_tuple([RecordName | RecordValueList]),
                                RowList1 = [#row{line = Line, record = Record} | RowList],
                                {NthListList, lists:keystore(RecordName, #sheet.name, SheetList, Sheet#sheet{row_list = RowList1})}
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

%% @doc 复制mask包裹的内容到指定文件, 不会影响文件原有内容

%% %%%%mask start%%%%%%

%% 复制的内容

%% %%%%mask start%%%%%%
copy_mask_body(Module, ToFile) ->
    ModuleStr = atom_to_list(Module),
    copy_mask_body(?XLSX2ERL_RECORD_START_MASK1(ModuleStr), ?XLSX2ERL_RECORD_END_MASK1(ModuleStr), "src/callback/" ++ ModuleStr ++ ".erl", ToFile).
copy_mask_body(MaskStart, MaskEnd, FromFile, ToFile) ->
    {ok, FromFD} = file:open(FromFile, [read]),
    case copy_mask_body_1(MaskStart, MaskEnd, FromFD, [], false) of
        [] ->
            file:close(FromFD),
            ok;
        MaskData ->
            file:close(FromFD),
            {ok, ToFD} = file:open(ToFile, [read, write]),
            case copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, [], false, false) of
                [] -> ok;
                Data ->
                    file:close(ToFD),
                    file:write_file(ToFile, Data)
            end
    end.

%% 先获取mask数据
copy_mask_body_1(MaskStart, MaskEnd, FromFD, MaskData, IsFindStart) ->
    case file:read_line(FromFD) of
        {ok, Data} ->
            case copy_mask_body_is_mask(Data, MaskStart) of
                true ->
                    copy_mask_body_1(MaskStart, MaskEnd, FromFD, [], true);
                false ->
                    case copy_mask_body_is_mask(Data, MaskEnd) of
                        true ->
                            MaskData;
                        false ->
                            case IsFindStart of
                                true ->
                                    copy_mask_body_1(MaskStart, MaskEnd, FromFD, [Data | MaskData], IsFindStart);
                                false ->
                                    copy_mask_body_1(MaskStart, MaskEnd, FromFD, [MaskStart], IsFindStart)
                            end
                    end
            end;
        eof ->
            [];
        {error, Error} ->
            io:format("copy_mask_body ~p error~n~p~n", [?LINE, Error]),
            []
    end.

%% 复制数据到to file
copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, ToData, IsFindStart, IsFinishCopy) ->
    case file:read_line(ToFD) of
        {ok, Data} ->
            case IsFinishCopy of
                true ->
                    copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, [Data | ToData], IsFindStart, IsFinishCopy);
                _ ->
                    case copy_mask_body_is_mask(Data, MaskStart) of
                        true ->
                            copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, MaskData ++ [Data | ToData], true, IsFinishCopy);
                        false ->
                            case copy_mask_body_is_mask(Data, MaskEnd) of
                                true ->
                                    copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, [Data | ToData], IsFindStart, true);
                                false ->
                                    case IsFindStart of
                                        true ->
                                            copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, ToData, IsFindStart, IsFinishCopy);
                                        false ->
                                            copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, [Data | ToData], IsFindStart, IsFinishCopy)
                                    end
                            end
                    end
            end;
        eof ->
            case IsFinishCopy of
                true ->
                    lists:reverse(ToData);
                _ ->
                    lists:reverse([MaskEnd, MaskData, MaskStart ++ "\n"] ++ "\n" ++ ToData)
            end;
        {error, Error} ->
            io:format("copy_mask_body ~p error~n~p~n", [?LINE, Error]),
            []
    end.

copy_mask_body_is_mask(_Data, []) ->
    true;
copy_mask_body_is_mask([H | T1], [H | T2]) ->
    copy_mask_body_is_mask(T1, T2);
copy_mask_body_is_mask(_Data, _Mask) ->
    false.

make_template(FileName) ->
    make_template(FileName, default).
make_template(FileName, OutFile) ->
    case string:split(filename:rootname(filename:basename(FileName)), "-") of
        [_, TagStr] ->
            ModuleStr = "xlsx2erl_" ++ TagStr,
            {RecordDef, CompileRecordDef} = make_template_record_def(FileName),
            Data =
                "-module(" ++ ModuleStr ++ ").\n\n"
            "-behaviour(xlsx2erl_export).\n\n"
            "-include(\"xlsx2erl.hrl\").\n\n"
            ?XLSX2ERL_RECORD_START_MASK1(ModuleStr) ++ "\n"
                ++ RecordDef ++
                ?XLSX2ERL_RECORD_END_MASK1(ModuleStr) ++ "\n\n"
            "-export([update_dets/1, compile/1, clean/1]).\n\n"
            "update_dets(#callback_args{filename = FileName}) ->\n"
            "    RecordDef = [\n"
                ++ CompileRecordDef ++ "\n"
            "    ],\n"
            "    case xlsx2erl:get_sheet_data(RecordDef, FileName) of\n"
            "        SheetList when is_list(SheetList) ->\n"
            "            Now = erlang:system_time(second),\n"
            "            dets:insert(?DETS_XLSX2ERL, #excel{name = ?MODULE, sheet_list = SheetList}),\n"
            "            dets:insert(?DETS_XLSX2ERL, ?XLSX2ERL_DETS_EXCEL_UPDATE2(?MODULE, Now));\n"
            "        _ ->\n"
            "            error\n"
            "    end.\n\n"
            "compile(#callback_args{}) ->\n"
            "    io:format(\"compile ~p~n\", [?MODULE]).\n\n"
            "clean(#callback_args{}) ->\n"
            "    io:format(\"clean~p~n\", [?MODULE]).\n\n",
            ErlFile =
                case OutFile of
                    default -> "src/callback/" ++ ModuleStr ++ ".erl";
                    _ -> OutFile
                end,
            io:format("write template file " ++ ErlFile ++ "~n"),
            file:write_file(ErlFile, Data);
        _ ->
            io:format("make template bad xlsx name ~ts~n", [FileName])
    end.

make_template_record_def(XlsxFile) ->
    RowHandler =
        fun(SheetName, [_Line, "KEY" | Row], {RD, CRD} = Acc) ->
            case string:split(SheetName, "-") of
                [_, RecordNameStr] ->
                    FieldList =
                        lists:foldr(fun(Field, Acc1) ->
                            case catch list_to_atom(Field) of
                                FieldAtom when is_atom(FieldAtom) ->
                                    [Field | Acc1];
                                _ ->
                                    Acc1
                            end
                                    end, [], Row),
                    {next_sheet, {
                        ["-record(" ++ RecordNameStr ++ ", {" ++ string:join(FieldList, ", ") ++ "}).\n" | RD],
                        [["        {" ++ RecordNameStr ++ ", record_info(fields, " ++ RecordNameStr ++ ")}"] | CRD]
                    }};
                _ ->
                    {next_sheet, Acc}
            end;
            (_, _, Acc) ->
                Acc
        end,
    case xlsx_reader:read(XlsxFile, {[], []}, RowHandler) of
        {error, Reason} ->
            io:format("get_sheet_name_list ~ts error~n~p~n", [XlsxFile, Reason]),
            error;
        {RecordDef, CompileRecordDef} ->
            {lists:reverse(RecordDef), string:join(lists:reverse(CompileRecordDef), ",\n")}
    end.