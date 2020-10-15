-module(xlsx2erl).

-include("xlsx2erl.hrl").

%% API exports
-export([main/1, get_sheet_data/2]).

%%====================================================================
%% API functions
%%====================================================================

%% escript 自动执行export文件下所有的erl文件
main(["compile", XlsxDir, ExportDir]) ->
    file:make_dir(ExportDir),
    filelib:fold_files(XlsxDir, ".xlsx", true,
        fun(FileName, _) ->
            case string:split(filename:rootname(filename:basename(FileName)), "-") of
                [_, ModuleStr] ->
                    Module = list_to_atom("xlsx2erl_" ++ ModuleStr),
                    Args = #callback_args{
                        filename = filename:absname(FileName),
                        export_path = ExportDir
                    },
                    case catch Module:compile(Args) of
                        {'EXIT', Error} ->
                            io:format("export ~p error~n~p~n~n", [Module, Error]);
                        _ ->
                            ok
                    end;
                _ ->
                    io:format("bad file name ~ts", [FileName])
            end
        end, []);
main(["clean", XlsxDir, ExportDir]) ->
    file:make_dir(ExportDir),
    filelib:fold_files(XlsxDir, ".xlsx", true,
        fun(FileName, _) ->
            case string:split(filename:rootname(filename:basename(FileName)), "-") of
                [_, ModuleStr] ->
                    Module = list_to_atom("xlsx2erl_" ++ ModuleStr),
                    Args = #callback_args{
                        filename = filename:absname(FileName),
                        export_path = ExportDir
                    },
                    case catch Module:clean(Args) of
                        {'EXIT', Error} ->
                            io:format("export ~p error~n~p~n~n", [Module, Error]);
                        _ ->
                            ok
                    end;
                _ ->
                    io:format("bad file name ~ts", [FileName])
            end
        end, []).

%% @doc 获取所有sheet的数据
-spec get_sheet_data([{RecordName :: atom(), RecordFieldList :: [atom()]}], file:filename()) -> error|[#sheet{}].
get_sheet_data(RecordDefList, XlsxFile) ->
    RowHandler =
        fun(SheetName, [Line, Key | Row], Acc) ->
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
                        case std_xlsx_make_record_nth(RecordFieldList, KeyMap, []) of
                            NthList when is_list(NthList) ->
                                [#sheet{name = SheetName, nth_list = NthList, row_list = []} | Acc];
                            Error ->
                                io:format("bad key in ~ts~n~p~n", [SheetName, Error]),
                                Acc
                        end;
                    "DATA" ->
                        case lists:keyfind(SheetName, #sheet.name, Acc) of
                            false ->% 未初始化key
                                Acc;
                            #sheet{nth_list = NthList, row_list = RowList} = Sheet ->
                                RecordValueList = std_xlsx_make_record_value(NthList, Row),
                                Record = list_to_tuple([RecordName | RecordValueList]),
                                RowList1 = [#row{line = Line, record = Record} | RowList],
                                lists:keystore(SheetName, #sheet.name, Acc, Sheet#sheet{row_list = RowList1})
                        end;
                    _ ->
                        Acc
                end,
            {next_row, Acc1};
            (_SheetName, _, Acc) ->
                {next_row, Acc}
        end,
    case xlsx_reader:read(XlsxFile, [], RowHandler) of
        {error, Reason} ->
            io:format("read ~ts error~n~p~n", [XlsxFile, Reason]),
            error;
        List ->
            List
    end.

std_xlsx_make_record_nth([], _, NthList) ->
    lists:reverse(NthList);
std_xlsx_make_record_nth([RecordField | T], KeyMap, NthList) ->
    case maps:get(RecordField, KeyMap, undefined) of
        undefined ->
            {not_exist, RecordField};
        Nth ->
            std_xlsx_make_record_nth(T, KeyMap, [Nth | NthList])
    end.

std_xlsx_make_record_value([], _) ->
    [];
std_xlsx_make_record_value([H | T], Row) ->
    [std_xlsx_make_record_value_1(H, Row) | std_xlsx_make_record_value(T, Row)].

std_xlsx_make_record_value_1(_N, []) ->
    [];
std_xlsx_make_record_value_1(N, [H | _]) when N =< 1 ->
    H;
std_xlsx_make_record_value_1(N, [_ | T]) ->
    std_xlsx_make_record_value_1(N - 1, T).

