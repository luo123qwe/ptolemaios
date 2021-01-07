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
-export([get_sheets/1]).

get_sheets(Filename) ->
    FD = get_fd(Filename),
    {ok, {_, Xml}} = zip:zip_get("xl/workbook.xml", FD),
    {ok, {_, _, SheetsList}} = get_element_from_xml("sheets", Xml, Filename),
    {ok, {_, RelXml}} = zip:zip_get("xl/_rels/workbook.xml.rels", FD),
    {ok, {_, _, RelList}} = get_element_from_xml("Relationships", RelXml, Filename),
    %% 构造rId和sheet.xml的map
    RIdMapping =
        lists:foldl(fun({_, KV, _}, Acc) ->
            Acc#{keyfind("Id", KV) => keyfind("Target", KV)}
                    end, #{}, RelList),
    WorkbookFullName = filename:basename(Filename, ".xlsx"),
    [_, WorkbookName] = string:split(WorkbookFullName, ?XLSX2ERL_SPLIT),
    lists:foldr(fun({_, KV, _}, Acc) ->
        FullName = keyfind("name", KV),
        case string:split(FullName, ?XLSX2ERL_SPLIT) of
            [_, Name] ->
                RId = keyfind("id", KV),
                [#xlsx2erl_sheet{
                    name = list_to_atom(Name), full_name = FullName,
                    workbook_name = list_to_atom(WorkbookName), filename = Filename,
                    module = ?XLSX2ERL_ERL_NAME2(WorkbookName, Name),
                    id = RId,% 因为先进行了正则匹配, "r:id"在这里会变成id
                    taget = maps:get(RId, RIdMapping)
                } | Acc];
            _ ->% 名字格式不对
                Acc
        end
                end, [], SheetsList).

get_element_from_xml(Tag, Xml, Filename) ->
    case re:run(Xml, "<" ++ Tag ++ ".*?>.*</" ++ Tag ++ ">", [{capture, all, binary}]) of
        {match, [TagXml]} ->
            case erlsom:simple_form(TagXml, []) of
                {ok, From, []} ->
                    {ok, From};
                Error ->
                    ?XLSX2ERL_ERROR2("get ~s in ~ts error:~n~p", [Tag, Filename, Error])
            end;
        _ ->
            ?XLSX2ERL_ERROR2("no ~s in ~ts~n~s", [Tag, Filename, Xml])
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

keyfind(K, L) ->
    case lists:keyfind(K, 1, L) of
        false -> ?XLSX2ERL_ERROR2("no ~w in ~w", [K, L]);
        {_, V} -> V
    end.