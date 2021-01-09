%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 读取xlsx
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xlsx2erl_reader).
-author("dominic").

-include("util.hrl").
-include("xlsx2erl.hrl").

%% API
-export([sheets_without_data/1, sheets_with_data/1, sheets_with_data/3]).

-spec sheets_without_data(file:filename()) -> [#xlsx2erl_sheet{}].
sheets_without_data(Filename) ->
    FD = get_fd(Filename),
    {ok, {_, WorkbookXml}} = zip:zip_get("xl/workbook.xml", FD),
    WorkbookForm = xml_to_form("workbook", WorkbookXml, Filename),
    SheetsForm = form_find("sheets", WorkbookForm),
    {ok, {_, RekXml}} = zip:zip_get("xl/_rels/workbook.xml.rels", FD),
    RelsForm = xml_to_form("Relationships", RekXml, Filename),
    %% 构造rId和sheet.xml的map
    RIdMapping =
        lists:foldl(fun({"Relationship", RelationshipAttr, _}, Acc) ->
            Acc#{attr_find("Id", RelationshipAttr) => attr_find("Target", RelationshipAttr)}
                    end, #{}, RelsForm),
    WorkbookFullName = filename:basename(Filename, ".xlsx"),
    [_, WorkbookName] = string:split(WorkbookFullName, ?XLSX2ERL_SPLIT),
    lists:foldr(fun({"sheet", SheetAttr, _}, Acc) ->
        FullName = attr_find("name", SheetAttr),
        case string:split(FullName, ?XLSX2ERL_SPLIT) of
            [_, Name] ->
                RId = attr_find("id", SheetAttr),
                [#xlsx2erl_sheet{
                    name = list_to_atom(Name), full_name = FullName,
                    workbook_name = list_to_atom(WorkbookName), filename = Filename,
                    module = ?XLSX2ERL_ERL_NAME2(WorkbookName, Name),
                    id = RId,% 因为先进行了正则匹配, "r:id"在这里会变成id
                    taget = "xl/" ++ maps:get(RId, RIdMapping)
                } | Acc];
            _ ->% 名字格式不对
                Acc
        end
                end, [], SheetsForm).

sheets_with_data(Filename) ->
    sheets_with_data(Filename, [{Key, 99999999} || Key <- ?XLSX2ERL_KEY_LIST], 99999999).

sheets_with_data([], _, _) -> [];
sheets_with_data([#xlsx2erl_sheet{filename = Filename, taget = Target} = Sheet | T], KeyTimesList, SearchLimit) ->
    FD = get_fd(Filename),
    {ok, {_, Xml}} = zip:zip_get(Target, FD),
    SheetForm = xml_to_form("worksheet", Xml, Filename),
    SheetDataForm = form_find("sheetData", SheetForm),
    ShareString = get_share_string(Filename),
    %% 构造行数据
    Key2RowListMap = sheets_with_data_key2row_list(SheetDataForm, KeyTimesList, SearchLimit, ShareString, #{}),
    Sheet1 = Sheet#xlsx2erl_sheet{data = Key2RowListMap},
    [Sheet1 | sheets_with_data(T, KeyTimesList, SearchLimit)];
sheets_with_data(Filename, KeyTimesList, SearchLimit) ->
    sheets_with_data(sheets_without_data(Filename), KeyTimesList, SearchLimit).

sheets_with_data_key2row_list([], _KeyTimesList, _SearchLimit, _ShareString, Key2RowListMap) ->
    maps:map(fun(_K, V) -> lists:reverse(V) end, Key2RowListMap);
sheets_with_data_key2row_list(_, [], _SearchLimit, _ShareString, Key2RowListMap) ->
    maps:map(fun(_K, V) -> lists:reverse(V) end, Key2RowListMap);
sheets_with_data_key2row_list(_, _KeyTimesList, SearchLimit, _ShareString, Key2RowListMap) when SearchLimit =< 0 ->
    maps:map(fun(_K, V) -> lists:reverse(V) end, Key2RowListMap);
sheets_with_data_key2row_list([{"row", _RowAttr, []} | T], KeyTimesList, SearchLimit, ShareString, Key2RowListMap) ->
    sheets_with_data_key2row_list(T, KeyTimesList, SearchLimit - 1, ShareString, Key2RowListMap);
sheets_with_data_key2row_list([{"row", RowAttr, [{"c", CAttrH, CFormH} | RowFormT]} | T], KeyTimesList, SearchLimit, ShareString, Key2RowListMap) ->
    %% 先看下是不是有效的行
    KeyNum =
        case get_cell_value(ShareString, CAttrH, CFormH) of
            undefined ->
                CellValueH = false;
            CellValueH ->
                lists:keyfind(CellValueH, 1, KeyTimesList)
        end,
    Key2RowListMap1 =
        case KeyNum of
            false ->
                Key2RowListMap;
            _ ->
                Line = list_to_integer(attr_find("r", RowAttr)),
                CellList =
                    lists:foldr(fun({"c", CAttr, CForm}, Acc2) ->
                        case get_cell_value(ShareString, CAttr, CForm) of
                            undefined ->
                                attr_find("s", CAttr),
                                %% 目前已知合并单元格除了左上角那格有东西其他都是[]
                                %% 合并的单元格的数据都一定不是需要导出的数据
                                %% 所以, 只记录左上角数据, 甚至不记录也可以
                                Acc2;
                            CellValue ->
                                ColumnIndex = get_column_index(attr_find("r", CAttr)),
                                [{ColumnIndex, CellValue} | Acc2]
                        end
                                end, [], RowFormT),
                RowList = maps:get(CellValueH, Key2RowListMap, []),
                RowList1 = [#xlsx2erl_raw_row{line = Line, data = CellList} | RowList],
                Key2RowListMap#{CellValueH => RowList1}
        end,
    KeyTimesList1 =
        case KeyNum of
            false -> KeyTimesList;
            {_, Times} ->
                ?IF(Times =< 1,
                    lists:keydelete(CellValueH, 1, KeyTimesList),
                    lists:keyreplace(CellValueH, 1, KeyTimesList, {CellValueH, Times - 1}))
        end,
    sheets_with_data_key2row_list(T, KeyTimesList1, SearchLimit - 1, ShareString, Key2RowListMap1).

get_cell_value(ShareString, CAttr, CForm) ->
    [V] = form_find("v", CForm, [undefined]),
    case attr_find("t", CAttr, "str") of
        "s" -> % ShareString
            maps:get(V, ShareString);
        "b" -> % boolean
            ?IF(V == "1", "TRUE", "FALSE");
        "str" ->
            V
    end.

xml_to_form(Tag, Xml, Filename) ->
    %% 先用正则把 namespace 去掉, 否者解析出来的标签会很长, 而且可能会爆内存
    %% 例如: {http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac}dyDescent
    %% 效率: 先re + simple_form > 直接simple_form
    Xml1 = re:replace(Xml, "\sxmlns.*?\".*?\"", "", [global, {return, binary}]),
    case erlsom:simple_form(Xml1, []) of
        {ok, {Tag, _Attr, Form}, []} ->
            Form;
        Error ->
            ?XLSX2ERL_ERROR2("xml_to_from ~ts error:~n~p", [Filename, Error])
    end.

get_fd(Filename) ->
    case get(?PD_XLSX2ERL_ZIP_FD1(Filename)) of
        undefined ->
            case zip:zip_open(Filename, [memory]) of
                {ok, FD} ->
                    put(?PD_XLSX2ERL_ZIP_FD1(Filename), FD),
                    FD;
                Error ->
                    ?XLSX2ERL_ERROR2("open ~ts error: ~w", [Error])
            end;
        FD ->
            FD
    end.

get_share_string(Filename) ->
    case get(?PD_XLSX2ERL_SHARE_STRING1(Filename)) of
        undefined ->
            FD = get_fd(Filename),
            {ok, {_, Xml}} = zip:zip_get("xl/sharedStrings.xml", FD),
            %% 把sst标签的xmlns部分去掉
            ShareStringForm = xml_to_form("sst", Xml, Filename),
            {_, Map} =
                lists:foldl(fun({"si", _, SIForm}, {I, M}) ->
                    case lists:keyfind("t", 1, SIForm) of
                        false ->
                            %% 看别人实现有这种情况
                            RForm = form_find("r", SIForm),
                            {_, _, [Str]} = form_find("t", RForm),
                            {I + 1, M#{integer_to_list(I) => Str}};
                        {_, _, [Str]} ->
                            {I + 1, M#{integer_to_list(I) => Str}}
                    end
                            end, {0, #{}}, ShareStringForm),
            put(?PD_XLSX2ERL_SHARE_STRING1(Filename), Map),
            Map;
        Map ->
            Map
    end.


attr_find(K, L) ->
    case lists:keyfind(K, 1, L) of
        false -> ?XLSX2ERL_ERROR2("no ~w in ~w", [K, L]);
        {_, V} -> V
    end.

attr_find(K, L, Default) ->
    case lists:keyfind(K, 1, L) of
        false -> Default;
        {_, V} -> V
    end.

form_find(K, L) ->
    case lists:keyfind(K, 1, L) of
        false -> ?XLSX2ERL_ERROR2("no ~w in ~w", [K, L]);
        {_, _Attr, Form} -> Form
    end.

form_find(K, L, Default) ->
    case lists:keyfind(K, 1, L) of
        false -> Default;
        {_, _Attr, Form} -> Form
    end.

get_column_index(ColumnStr) ->
    {match, [WordStr]} = re:run(ColumnStr, "[A-Z]+", [{capture, all, list}]),
    {_, Sum} =
        lists:foldr(fun(Char, {Exp, S}) ->
            {Exp * 26, (Char - $A + 1) * Exp + S}
                    end, {1, 0}, WordStr),
    Sum.
    