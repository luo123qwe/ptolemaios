-module(xlsx2erl_test).

-behaviour(xlsx2erl_callback).

-include("xlsx2erl.hrl").

%%%%%%%%%%%xlsx2erl_test record define start%%%%%%%%%%%%%%%%%
-record(goods, {id, type, name, price, resolve_reward}).
-record(equip, {id, attr}).
%%%%%%%%%%%xlsx2erl_test record define end%%%%%%%%%%%%%%%%%%%

-export([update_dets/1, compile/1, clean/1]).

update_dets(FileName) ->
    RecordDef = [
        {goods, record_info(fields, goods)},
        {equip, record_info(fields, equip)}
    ],
    case xlsx2erl:get_sheet_data(RecordDef, FileName) of
        SheetList when is_list(SheetList) ->
            %% 转换成erlang数据, 同时可以检查数据是否正确
            SheetList1 = update_dets_convert(SheetList),
            Now = erlang:localtime(),
            dets:insert(?DETS_XLSX2ERL1(?MODULE), #xlsx2erl_excel{name = ?MODULE, sheet_list = SheetList1}),
            dets:insert(?DETS_XLSX2ERL1(?MODULE), ?XLSX2ERL_DETS_EXCEL_UPDATE2(?MODULE, Now));
        _ ->
            error
    end.

update_dets_convert([]) ->
    [];
update_dets_convert([#xlsx2erl_sheet{name = goods, row_list = RowList} = H | T]) ->
    RowList1 = update_dets_convert_record_goods(RowList, H, record_info(fields, goods), []),
    [H#xlsx2erl_sheet{row_list = RowList1} | update_dets_convert(T)];
update_dets_convert([#xlsx2erl_sheet{name = equip, row_list = RowList} = H | T]) ->
    RowList1 = update_dets_convert_record_equip(RowList, H, record_info(fields, equip), []),
    [H#xlsx2erl_sheet{row_list = RowList1} | update_dets_convert(T)].

%% todo 选择转换类型
update_dets_convert_record_goods([], _Sheet, _RecordDef, RowList) ->
    RowList;
update_dets_convert_record_goods([H | T], Sheet, RecordDef, RowList) ->
    RowList1 =
        [H#xlsx2erl_row{record = #goods{
            id = xlsx2erl_util:convert_int(#goods.id, RecordDef, H, Sheet),
            type = xlsx2erl_util:convert_int(#goods.type, RecordDef, H, Sheet),
            name = xlsx2erl_util:convert_bin(#goods.name, RecordDef, H, Sheet),
            price = xlsx2erl_util:convert_float(#goods.price, RecordDef, H, Sheet),
            resolve_reward = xlsx2erl_util:convert_json(#goods.resolve_reward, RecordDef, H, Sheet)
        }} | RowList],
    update_dets_convert_record_goods(T, Sheet, RecordDef, RowList1).

update_dets_convert_record_equip([], _Sheet, _RecordDef, RowList) ->
    RowList;
update_dets_convert_record_equip([H | T], Sheet, RecordDef, RowList) ->
    RowList1 =
        [H#xlsx2erl_row{record = #equip{
            id = xlsx2erl_util:convert_bin(#equip.id, RecordDef, H, Sheet),
            attr = xlsx2erl_util:convert_json(#equip.attr, RecordDef, H, Sheet)
        }} | RowList],
    update_dets_convert_record_equip(T, Sheet, RecordDef, RowList1).

compile(#xlsx2erl_callback_args{export_path = ExportPath} = Args) ->
    #xlsx2erl_excel{sheet_list = SheetList} = xlsx2erl:get_excel(?MODULE),
    xlsx2erl:copy_mask_body(?MODULE, ExportPath ++ "/" ++ ?XLSX2ERL_DEFAULT_HRL),
    do_compile(SheetList, Args).

do_compile([], _Args) ->
    ok;
do_compile([#xlsx2erl_sheet{name = goods, row_list = RowList} = Sheet | T], #xlsx2erl_callback_args{export_path = ExportPath} = Args) ->
    Head =
        "-module(" ++ ?XLSX2ERL_DEFAULT_DATA_MODULE(goods) ++ ").\n\n"
    "-include(\"" ++ ?XLSX2ERL_DEFAULT_HRL ++ "\").\n\n"
    "-export([get/1]).\n\n",
    Body =
        lists:map(fun(#xlsx2erl_row{record = Record} = Row) ->
            %% 转换成[{物品id, 数量}]
            ResolveReward =
                lists:foldl(fun(Map, Acc) ->
                    try
                        #{<<"gid">> := Gid, <<"num">> := Num} = Map,
                        [{Gid, Num} | Acc]
                    catch
                        _:_ ->
                            ?XLSX2ERL_ERROR4(Sheet, Row, "resolve_reward: ~ts", [jsx:encode(Map)]),
                            exit(badarg)
                    end
                            end, [], Record#goods.resolve_reward),
            "get(" ++ xlsx2erl_util:to_iolist(Record#goods.id) ++ ") -> \n" ++
                "    #goods{\n"
                "        id = " ++ xlsx2erl_util:to_iolist(Record#goods.id) ++ ", \n" ++
                "        type = " ++ xlsx2erl_util:to_iolist(Record#goods.type) ++ ", \n" ++
                "        name = " ++ xlsx2erl_util:to_iolist(Record#goods.name) ++ ", \n" ++
                "        price = " ++ xlsx2erl_util:to_iolist(Record#goods.price) ++ ", \n" ++
                "        resolve_reward = " ++ xlsx2erl_util:to_iolist(ResolveReward) ++ "\n" ++
                "};\n"
                  end, RowList),
    Tail =
        "get(_) -> undefined.",
    file:write_file(ExportPath ++ "/data_goods.erl", [Head, Body, Tail]),
    do_compile(T, Args);
do_compile([#xlsx2erl_sheet{name = equip, row_list = RowList} = Sheet | T], #xlsx2erl_callback_args{export_path = ExportPath} = Args) ->
    Head =
        "-module(" ++ ?XLSX2ERL_DEFAULT_DATA_MODULE(equip) ++ ").\n\n"
    "-include(\"" ++ ?XLSX2ERL_DEFAULT_HRL ++ "\").\n\n"
    "-export([get/1]).\n\n",
    Body =
        lists:map(fun(#xlsx2erl_row{record = Record} = Row) ->
            %% 转换成 [{属性名(atom), 属性值}]
            Attr =
                try
                    [{binary_to_atom(K), V} || {K, V} <- maps:to_list(Record#equip.attr)]
                catch
                    _:_ ->
                        ?XLSX2ERL_ERROR4(Sheet, Row, "attr: ~ts", [jsx:encode(Record#equip.attr)]),
                        exit(badarg)
                end,
            [
                    "get(" ++ xlsx2erl_util:to_iolist(Record#equip.id) ++ ") ->\n" ++
                    "    #equip{"
                    "id = " ++ xlsx2erl_util:to_iolist(Record#equip.id) ++ ", " ++
                    "attr = " ++ xlsx2erl_util:to_iolist(Attr) ++
                    "};\n"
            ]
                  end, RowList),
    Tail =
        "get(_) -> undefined.",
    file:write_file(ExportPath ++ "/data_equip.erl", [Head, Body, Tail]),
    do_compile(T, Args).

clean(#xlsx2erl_callback_args{export_path = ExportPath}) ->
    file:delete(ExportPath ++ "/" ++ ?XLSX2ERL_DEFAULT_HRL),
    catch dets:close(?DETS_XLSX2ERL1(?MODULE)),
    file:delete(?DETS_PATH ++ "/" ++ ?MODULE_STRING),
    file:delete(ExportPath ++ "/data_goods.erl"),
    file:delete(ExportPath ++ "/data_equip.erl").
