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
%% 需要配合进程字典使用
-export([to_bin/2, to_int/2, to_float/2, to_term/2, to_json/2]).

-export([to_iolist/1, ensure_dets/1]).

-export([copy_mask_body/3, copy_mask_body/4, write_mask_body/3, delete_mask_body/2]).

%% @doc 二进制文本
-spec to_bin(non_neg_integer(), #xlsx2erl_row{}) -> binary().
to_bin(Index, Row) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try list_to_binary(Field)
    catch
        _:_ ->
            RecordDef = get(?PD_XLSX2ERL_FIELD_DEF),
            ?XLSX2ERL_PD_ERROR2("需要文本 ~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc 整数
-spec to_int(non_neg_integer(), #xlsx2erl_row{}) -> integer().
to_int(Index, Row) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try list_to_integer(Field)
    catch
        _:_ ->
            RecordDef = get(?PD_XLSX2ERL_FIELD_DEF),
            ?XLSX2ERL_PD_ERROR2("需要整数 ~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc 浮点数
-spec to_float(non_neg_integer(), #xlsx2erl_row{}) -> float().
to_float(Index, Row) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try
        case catch list_to_float(Field) of
            Float when is_float(Float) -> Float;
            _ -> list_to_integer(Field)
        end
    catch
        _:_ ->
            RecordDef = get(?PD_XLSX2ERL_FIELD_DEF),
            ?XLSX2ERL_PD_ERROR2("需要浮点数 ~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc json
-spec to_json(non_neg_integer(), #xlsx2erl_row{}) -> jsx:json_term().
to_json(Index, Row) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try jsx:decode(list_to_binary(Field))
    catch
        _:_ ->
            RecordDef = get(?PD_XLSX2ERL_FIELD_DEF),
            ?XLSX2ERL_PD_ERROR2("需要JSON ~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc erlang结构
-spec to_term(non_neg_integer(), #xlsx2erl_row{}) -> term().
to_term(Index, Row) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try eval(Field)
    catch
        _:_ ->
            RecordDef = get(?PD_XLSX2ERL_FIELD_DEF),
            ?XLSX2ERL_PD_ERROR2("需要ERL结构 ~p: ~p", [lists:nth(Index - 1, RecordDef), Field])
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

%% @doc 转换成erlang格式, 返回IOList
%%
%% 文本请传入binary, 转换成<<X/utf8>>
to_iolist(Binary) when is_binary(Binary) ->
    ["<<\"", re:replace(re:replace(Binary, "\\\\", "\\\\\\\\", [global]), "\"", "\\\\\"", [global]), "\"/utf8>>"];
to_iolist(Term) ->
    io_lib:format("~999999p", [Term]).

%% @doc 复制mask包裹的内容到指定文件, 不会影响文件原有内容, 搜索到第一个mask end就停止

%% 如果ToFile以-endif.(后续可以包含([ \t\n]*)+|(%.*))结尾, 且没有复制过, 则复制到-endif.前

%% %%%%mask start%%%%%%

%% 复制的内容

%% %%%%mask end%%%%%%
copy_mask_body(Module, Tag, ToFile) ->
    MaskStart = ?XLSX2ERL_MASK_START1(Tag),
    MaskEnd = ?XLSX2ERL_MASK_END1(Tag),
    ModuleStr = atom_to_list(Module),
    FromFile = "include/" ++ ModuleStr ++ ".hrl",
    copy_mask_body(MaskStart, MaskEnd, FromFile, ToFile).
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
            ?XLSX2ERL_ERROR2("~p", [Error])
    end.

%% 一行行读
copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, ToData, IsFindStart, IsFinishCopy) ->
    case file:read_line(ToFD) of
        {ok, "-endif." ++ _ = Data} when IsFinishCopy == false, IsFindStart == false ->
            %% 如果最后是-endif., 且没有复制
            %% 把内容复制到-endif.前
            copy_mask_body_3(MaskStart, MaskEnd, ToFD, MaskData, ToData, [Data]);
        {ok, Data} ->
            {ToData1, IsFindStart1, IsFinishCopy1} =
                copy_mask_body_4(MaskStart, MaskEnd, Data, MaskData, ToData, IsFindStart, IsFinishCopy),
            copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, ToData1, IsFindStart1, IsFinishCopy1);
        eof ->
            case IsFinishCopy of
                true ->
                    lists:reverse(ToData);
                _ ->
                    lists:reverse([MaskEnd ++ "\n", lists:reverse(MaskData), MaskStart ++ "\n"] ++ "\n" ++ ToData)
            end;
        {error, Error} ->
            ?XLSX2ERL_ERROR2("~p", [Error])
    end.

%% 剩下的内容是不是erlang代码
copy_mask_body_3(MaskStart, MaskEnd, ToFD, MaskData, ToData, NewToData) ->
    case file:read_line(ToFD) of
        {ok, "-endif." ++ _ = Data} ->
            %% 如果最后是-endif., 且没有复制
            %% 把内容复制到-endif.前
            copy_mask_body_3(MaskStart, MaskEnd, ToFD, MaskData, NewToData ++ ToData, [Data]);
        {ok, Data} ->
            case re:run(Data, "([ \t\n]*)+|(%.*)", [{capture, first, index}]) of
                nomatch ->
                    {ToData1, IsFindStart1, IsFinishCopy1} =
                        copy_mask_body_4(MaskStart, MaskEnd, Data, MaskData, NewToData ++ ToData, false, false),
                    copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData, ToData1, IsFindStart1, IsFinishCopy1);
                _ ->
                    copy_mask_body_3(MaskStart, MaskEnd, ToFD, MaskData, ToData, [Data | NewToData])
            end;
        eof ->
            lists:reverse(NewToData ++ [MaskEnd ++ "\n", lists:reverse(MaskData), MaskStart ++ "\n"] ++ "\n" ++ ToData);
        {error, Error} ->
            ?XLSX2ERL_ERROR2("~p", [Error])
    end.

%% 复制到ToData
copy_mask_body_4(MaskStart, MaskEnd, Data, MaskData, ToData, IsFindStart, IsFinishCopy) ->
    case IsFinishCopy of
        true ->
            {[Data | ToData], IsFindStart, IsFinishCopy};
        _ ->
            case copy_mask_body_is_mask(Data, MaskStart) of
                true ->% 开始复制
                    {MaskData ++ [Data | ToData], true, IsFinishCopy};
                false ->
                    case copy_mask_body_is_mask(Data, MaskEnd) of
                        true ->% 结束复制
                            {[Data | ToData], IsFindStart, true};
                        false ->
                            case IsFindStart of
                                true ->
                                    {ToData, IsFindStart, IsFinishCopy};
                                false ->
                                    {[Data | ToData], IsFindStart, IsFinishCopy}
                            end
                    end
            end
    end.

copy_mask_body_is_mask([$\n], []) ->
    true;
copy_mask_body_is_mask([], []) ->% end可能没有换行
    true;
copy_mask_body_is_mask([H | T1], [H | T2]) ->
    copy_mask_body_is_mask(T1, T2);
copy_mask_body_is_mask(_Data, _Mask) ->
    false.

%% @doc 写指定内容到文件并用mask包裹, 原理同copy_mask_body
write_mask_body(MaskData, Tag, ToFile) ->
    MaskStart = ?XLSX2ERL_MASK_START1(Tag),
    MaskEnd = ?XLSX2ERL_MASK_END1(Tag),
    {ok, ToFD} = file:open(ToFile, [read, write]),
    %% 后面会翻转
    MaskData1 = ["\n", MaskData],
    case copy_mask_body_2(MaskStart, MaskEnd, ToFD, MaskData1, [], false, false) of
        [] -> ok;
        Data ->
            file:close(ToFD),
            ok = file:write_file(ToFile, Data)
    end.


%% @doc 删除指定文件mask包裹的内容, 原理同copy_mask_body
delete_mask_body(Tag, ToFile) ->
    MaskStart = ?XLSX2ERL_MASK_START1(Tag),
    MaskEnd = ?XLSX2ERL_MASK_END1(Tag),
    {ok, ToFD} = file:open(ToFile, [read, write]),
    case delete_mask_body_1(ToFD, MaskStart, MaskEnd, false, false, []) of
        skip ->
            file:close(ToFD);
        NewData ->
            file:close(ToFD),
            file:write_file(ToFile, NewData)
    end.

delete_mask_body_1(ToFD, MaskStart, MaskEnd, IsStart, IsEnd, ToData) ->
    case file:read_line(ToFD) of
        {ok, Data} when IsEnd ->
            delete_mask_body_1(ToFD, MaskStart, MaskEnd, IsStart, IsEnd, [Data | ToData]);
        {ok, Data} when IsStart ->
            case copy_mask_body_is_mask(Data, MaskEnd) of
                true ->% 结束删除, 读剩下的
                    delete_mask_body_1(ToFD, MaskStart, MaskEnd, IsStart, true, ToData);
                _ ->
                    delete_mask_body_1(ToFD, MaskStart, MaskEnd, IsStart, IsEnd, ToData)
            end;
        {ok, Data} ->
            case copy_mask_body_is_mask(Data, MaskStart) of
                true ->% 开始删除
                    %% 如果前一行是换行, 把换行符也删了
                    case ToData of
                        [[$\n] | ToData1] ->
                            delete_mask_body_1(ToFD, MaskStart, MaskEnd, true, IsEnd, ToData1);
                        _ ->
                            delete_mask_body_1(ToFD, MaskStart, MaskEnd, true, IsEnd, ToData)
                    end;
                _ ->
                    delete_mask_body_1(ToFD, MaskStart, MaskEnd, IsStart, IsEnd, [Data | ToData])
            end;
        eof when IsEnd ->
            lists:reverse(ToData);
        eof ->% 没有东西可以删除
            skip;
        {error, Error} ->
            ?XLSX2ERL_ERROR2("~p", [Error])
    end.

%% @doc 确保dets开启
-spec ensure_dets(atom()) -> any().
ensure_dets(Name) ->
    case dets:info(Name) of
        undefined ->
            {ok, Name} = dets:open_file(Name, [{file, ?XLSX2ERL_DETS_PATH ++ "/" ++ atom_to_list(Name) ++ ".dets"}, {keypos, 1}]);
        _ -> ok
    end.