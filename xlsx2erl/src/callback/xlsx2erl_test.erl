-module(xlsx2erl_test).

-behaviour(xlsx2erl_callback).

-include("xlsx2erl.hrl").

%%%%%%%%%%%xlsx2erl_test record define start%%%%%%%%%%%%%%%%%%%
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
            dets:insert(?DETS_XLSX2ERL1(?MODULE), #excel{name = ?MODULE, sheet_list = SheetList1}),
            dets:insert(?DETS_XLSX2ERL1(?MODULE), ?XLSX2ERL_DETS_EXCEL_UPDATE2(?MODULE, Now));
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
update_dets_convert_record(#goods{}) ->
    #goods{
        id = xlsx2erl_util:convert_int(#goods.id),
        type = xlsx2erl_util:convert_int(#goods.type),
        name = xlsx2erl_util:convert_bin(#goods.name),
        price = xlsx2erl_util:convert_float(#goods.price),
        resolve_reward = xlsx2erl_util:convert_json(#goods.resolve_reward)
    };
update_dets_convert_record(#equip{}) ->
    #equip{
        id = xlsx2erl_util:convert_bin(#equip.id),
        attr = xlsx2erl_util:convert_json(#equip.attr)
    }.

compile(#callback_args{export_path = ExportPath} = Args) ->
    #excel{sheet_list = SheetList} = xlsx2erl:get_excel(?MODULE),
    xlsx2erl:copy_mask_body(?MODULE, ExportPath ++ "/xlsx2erl_test.hrl"),
    do_compile(SheetList, Args).

do_compile([], _Args) ->
    ok;
do_compile([#sheet{name = goods, row_list = RowList} = Sheet | T], #callback_args{export_path = ExportPath} = Args) ->
    Head =
        "-module(data_goods).\n\n"
        "-include(\"xlsx2erl_test.hrl\").\n\n"
        "-export([get/1]).\n\n",
    Body =
        lists:map(fun(#row{record = Record} = Row) ->
            %% 转换成[{物品id, 数量}]
            ResolveReward =
                lists:foldl(fun(Map, Acc) ->
                    try
                        #{<<"gid">> := Gid, <<"num">> := Num} = Map,
                        [{Gid, Num} | Acc]
                    catch
                        _:_ ->
                            ?XLSX2ERL_ERROR(Sheet, Row, "resolve_reward: ~ts~n", [jsx:encode(Map)]),
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
do_compile([#sheet{name = equip, row_list = RowList} = Sheet | T], #callback_args{export_path = ExportPath} = Args) ->
    Head =
        "-module(data_equip).\n\n"
        "-include(\"xlsx2erl_test.hrl\").\n\n"
        "-export([get/1]).\n\n",
    Body =
        lists:map(fun(#row{record = Record} = Row) ->
            %% 转换成 [{属性名(atom), 属性值}]
            Attr =
                try
                    [{binary_to_atom(K), V} || {K, V} <- maps:to_list(Record#equip.attr)]
                catch
                    _:_ ->
                        ?XLSX2ERL_ERROR(Sheet, Row, "attr: ~ts~n", [jsx:encode(Record#equip.attr)]),
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

clean(#callback_args{export_path = ExportPath}) ->
    file:delete(ExportPath ++ "/xlsx2erl_test.hrl"),
    file:delete(ExportPath ++ "/data_goods.erl"),
    file:delete(ExportPath ++ "/data_equip.erl").
