%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xlsx2erl_util).
-author("dominic").

-include("xlsx2erl.hrl").

%% 转换文本到erl结构
-export([convert_bin/4, convert_int/4, convert_float/4, convert_term/4, convert_json/4]).


-export([to_iolist/1, copy_mask_body/2, copy_mask_body/4, ensure_dets/1, get_excel/1]).

%% @doc 二进制文本
-spec convert_bin(non_neg_integer(), [atom()], #xlsx2erl_row{}, #xlsx2erl_sheet{}) -> binary().
convert_bin(Index, RecordDef, Row, Sheet) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try unicode:characters_to_binary(Field)
    catch
        _:_ ->
            ?XLSX2ERL_ERROR4(Sheet, Row, "~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc 整数
-spec convert_int(non_neg_integer(), [atom()], #xlsx2erl_row{}, #xlsx2erl_sheet{}) -> integer().
convert_int(Index, RecordDef, Row, Sheet) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try list_to_integer(Field)
    catch
        _:_ ->
            ?XLSX2ERL_ERROR4(Sheet, Row, "~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc 浮点数
-spec convert_float(non_neg_integer(), [atom()], #xlsx2erl_row{}, #xlsx2erl_sheet{}) -> float().
convert_float(Index, RecordDef, Row, Sheet) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try
        case catch list_to_float(Field) of
            Float when is_float(Float) -> Float;
            _ -> list_to_integer(Field)
        end
    catch
        _:_ ->
            ?XLSX2ERL_ERROR4(Sheet, Row, "~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc json
-spec convert_json(non_neg_integer(), [atom()], #xlsx2erl_row{}, #xlsx2erl_sheet{}) -> jsx:json_term().
convert_json(Index, RecordDef, Row, Sheet) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try jsx:decode(unicode:characters_to_binary(Field))
    catch
        _:_ ->
            ?XLSX2ERL_ERROR4(Sheet, Row, "~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc erlang结构
-spec convert_term(non_neg_integer(), [atom()], #xlsx2erl_row{}, #xlsx2erl_sheet{}) -> term().
convert_term(Index, RecordDef, Row, Sheet) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try eval(Field)
    catch
        _:_ ->
            ?XLSX2ERL_ERROR4(Sheet, Row, "~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

eval(Str) when is_binary(Str) ->
    eval(binary_to_list(Str));
eval(Str) when is_list(Str) ->
    {ok, Tokens, _} = erl_scan:string(eval_fix_string(Str)),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Exprs, []),
    Value.

eval_fix_string([]) ->
    [$.];
eval_fix_string([$.]) ->
    [$.];
eval_fix_string([H | T]) ->
    [H | eval_fix_string(T)].

%% @doc 转换成IOList, 文本请传入binary
to_iolist(Binary) when is_binary(Binary) ->
    List = unicode:characters_to_list(Binary),
    %% 文本中含有 " 需要转换成 \", \ 转换成 \\
    %% todo 这里不知道会不会卡?
    case lists:any(fun(Char) -> Char > 255 end, List) of
        true ->% unicode
            ["<<\"", re:replace(re:replace(Binary, "\\\\", "\\\\\\\\", [global]), "\"", "\\\\\"", [global]), "\"/utf8>>"];
        _ ->
            ["<<\"", re:replace(re:replace(List, "\\\\", "\\\\\\\\", [global]), "\"", "\\\\\"", [global]), "\"/utf8>>"]
    end;
to_iolist(Term) ->
    io_lib:format("~w", [Term]).

%% @doc 复制mask包裹的内容到指定文件, 不会影响文件原有内容, 搜索到第一个mask end就停止

%% %%%%mask start%%%%%%

%% 复制的内容

%% %%%%mask end%%%%%%
copy_mask_body(Module, ToFile) ->
    ModuleStr = atom_to_list(Module),
    copy_mask_body(?XLSX2ERL_RECORD_START_MASK1(ModuleStr), ?XLSX2ERL_RECORD_END_MASK1(ModuleStr), "include/" ++ ModuleStr ++ ".hrl", ToFile).
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
                    ok = file:write_file(ToFile, Data)
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
                        true ->% 开始复制
                            copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, MaskData ++ [Data | ToData], true, IsFinishCopy);
                        false ->
                            case copy_mask_body_is_mask(Data, MaskEnd) of
                                true ->% 结束复制
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

copy_mask_body_is_mask([$\n], []) ->
    true;
copy_mask_body_is_mask([], []) ->% end可能没有换行
    true;
copy_mask_body_is_mask([H | T1], [H | T2]) ->
    copy_mask_body_is_mask(T1, T2);
copy_mask_body_is_mask(_Data, _Mask) ->
    false.

%% @doc 确保dets开启
-spec ensure_dets(atom()) -> any().
ensure_dets(Module) ->
    case dets:info(?DETS_XLSX2ERL1(Module)) of
        undefined ->
            {ok, ?DETS_XLSX2ERL1(Module)} = dets:open_file(?DETS_XLSX2ERL1(Module), [{file, ?DETS_PATH ++ "/" ++ atom_to_list(Module) ++ ".dets"}, {keypos, 2}]);
        _ -> ok
    end.

%% @doc 获取excel
-spec get_excel(atom()) -> undefined|#xlsx2erl_excel{}.
get_excel(Module) ->
    ensure_dets(?DETS_XLSX2ERL1(Module)),
    case dets:lookup(?DETS_XLSX2ERL1(Module), Module) of
        [Excel] -> Excel;
        _ -> undefined
    end.