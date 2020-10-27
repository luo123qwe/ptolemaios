-module(xlsx2erl_test).

-behaviour(xlsx2erl_callback).

-include("xlsx2erl.hrl").

%%%%%%%%%%%xlsx2erl_test record define start%%%%%%%%%%%%%%%%%%%
-record(theme_info, {id, type, name, price, resolve_reward}).
-record(theme_symbol, {id, attr}).
%%%%%%%%%%%xlsx2erl_test record define end%%%%%%%%%%%%%%%%%%%

-export([update_dets/1, compile/1, clean/1]).

update_dets(FileName) ->
    RecordDef = [
        {theme_info, record_info(fields, theme_info)},
        {theme_symbol, record_info(fields, theme_symbol)}
    ],
    case xlsx2erl:get_sheet_data(RecordDef, FileName) of
        SheetList when is_list(SheetList) ->
            %% 转换成erlang数据, 同时可以检查数据是否正确
            SheetList1 = update_dets_convert(SheetList),
            Now = erlang:localtime(),
            dets:insert(?DETS_XLSX2ERL, #excel{name = ?MODULE, sheet_list = SheetList1}),
            dets:insert(?DETS_XLSX2ERL, ?XLSX2ERL_DETS_EXCEL_UPDATE2(?MODULE, Now));
        _ ->
            error
    end.

update_dets_convert([]) ->
    [];
update_dets_convert([#sheet{row_list = RowList} = H | T]) ->
    RowList1 =
        lists:foldl(fun(Row, Acc) ->
            xlsx2erl_util:set_row(Row),
            Acc1 = [Row#row{record = update_dets_convert_record(Row#row.record)} | Acc],
            xlsx2erl_util:clean_row(),
            Acc1
                    end, [], RowList),
    [H#sheet{row_list = RowList1} | update_dets_convert(T)].

%% todo 在这里添加数据转换代码
update_dets_convert_record(#theme_info{}) ->
    #theme_info{
        id = xlsx2erl_util:convert_bin(#theme_info.id),
        type = xlsx2erl_util:convert_bin(#theme_info.type),
        name = xlsx2erl_util:convert_bin(#theme_info.name),
        price = xlsx2erl_util:convert_bin(#theme_info.price),
        resolve_reward = xlsx2erl_util:convert_bin(#theme_info.resolve_reward)
    };
update_dets_convert_record(#theme_symbol{}) ->
    #theme_symbol{
        id = xlsx2erl_util:convert_bin(#theme_symbol.id),
        attr = xlsx2erl_util:convert_bin(#theme_symbol.attr),
        c = xlsx2erl_util:convert_bin(#theme_symbol.c),
        d = xlsx2erl_util:convert_bin(#theme_symbol.d),
        e = xlsx2erl_util:convert_bin(#theme_symbol.e),
        f = xlsx2erl_util:convert_bin(#theme_symbol.f),
        g = xlsx2erl_util:convert_bin(#theme_symbol.g),
        h = xlsx2erl_util:convert_bin(#theme_symbol.h),
        i = xlsx2erl_util:convert_bin(#theme_symbol.i),
        j = xlsx2erl_util:convert_bin(#theme_symbol.j),
        k = xlsx2erl_util:convert_bin(#theme_symbol.k),
        l = xlsx2erl_util:convert_bin(#theme_symbol.l),
        m = xlsx2erl_util:convert_bin(#theme_symbol.m),
        n = xlsx2erl_util:convert_bin(#theme_symbol.n),
        o = xlsx2erl_util:convert_bin(#theme_symbol.o)
    }.

compile(#callback_args{export_path = ExportPath} = Args) ->
    #excel{sheet_list = SheetList} = xlsx2erl:get_excel(?MODULE),
    xlsx2erl:copy_mask_body(?MODULE, ExportPath ++ "/xlsx2erl_test.hrl"),
    do_compile(SheetList, Args).

do_compile([], _Args) ->
    ok;
do_compile([#sheet{name = theme_info, row_list = RowList} | T], #callback_args{export_path = ExportPath} = Args) ->
    Head =
        "-module(data_theme_info).\n\n"
        "-include(\"xlsx2erl_test.hrl\").\n\n"
        "-export([get/1]).\n\n",
    Body =
        lists:map(fun(#row{record = Record}) ->
            "get(" ++ xlsx2erl_util:to_iolist(Record#theme_info.id) ++ ") ->\n" ++
                "    #theme_info{\n"
                "        a = " ++ xlsx2erl_util:to_iolist(Record#theme_info.id) ++ ",\n" ++
                "        b = " ++ xlsx2erl_util:to_iolist(Record#theme_info.type) ++ ",\n" ++
                "        c = " ++ xlsx2erl_util:to_iolist(Record#theme_info.name) ++ ",\n" ++
                "        d = " ++ xlsx2erl_util:to_iolist(Record#theme_info.price) ++ ",\n" ++
                "        e = " ++ xlsx2erl_util:to_iolist(Record#theme_info.resolve_reward) ++ ",\n" ++
                "        f = " ++ xlsx2erl_util:to_iolist(Record#theme_info.f) ++ "\n" ++
                "};\n"
                  end, RowList),
    Tail =
        "get(_) -> undefined.",
    file:write_file(ExportPath ++ "/data_theme_info.erl", [Head, Body, Tail]),
    do_compile(T, Args);
do_compile([#sheet{name = theme_symbol, row_list = RowList} | T], #callback_args{export_path = ExportPath} = Args) ->
    Head =
        "-module(data_theme_symbol).\n\n"
        "-include(\"xlsx2erl_test.hrl\").\n\n"
        "-export([get/1]).\n\n",
    Body =
        lists:map(fun(#row{record = Record}) ->
            [
                    "get(" ++ xlsx2erl_util:to_iolist(Record#theme_symbol.id) ++ ") ->\n" ++
                    "    #theme_symbol{"
                    "a = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.id) ++ ", " ++
                    "b = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.attr) ++ ", " ++
                    "c = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.c) ++ ", " ++
                    "d = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.d) ++ ", " ++
                    "e = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.e) ++ ", " ++
                    "f = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.f) ++ ", " ++
                    "g = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.g) ++ ", " ++
                    "h = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.h) ++ ", " ++
                    "i = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.i) ++ ", " ++
                    "j = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.j) ++ ", " ++
                    "k = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.k) ++ ", " ++
                    "l = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.l) ++ ", " ++
                    "m = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.m) ++ ", " ++
                    "n = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.n) ++ ", " ++
                    "o = " ++ xlsx2erl_util:to_iolist(Record#theme_symbol.o) ++
                    "};\n"
            ]
                  end, RowList),
    Tail =
        "get(_) -> undefined.",
    file:write_file(ExportPath ++ "/data_theme_symbol.erl", [Head, Body, Tail]),
    do_compile(T, Args).

clean(#callback_args{export_path = ExportPath}) ->
    file:delete(ExportPath ++ "/xlsx2erl_test.hrl"),
    file:delete(ExportPath ++ "/data_theme_info.erl"),
    file:delete(ExportPath ++ "/data_theme_symbol.erl").
