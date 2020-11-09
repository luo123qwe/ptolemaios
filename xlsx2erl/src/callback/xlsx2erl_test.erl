-module(xlsx2erl_test).

-behaviour(xlsx2erl_callback).

-include("xlsx2erl.hrl").
-include("xlsx2erl_test.hrl").

-define(DETS_DICT1(Name), {Name, dict}).

-export([get_goods/0, get_equip/0]).

-export([update_dets/1, compile/1, clean/1]).

update_dets(FileName) ->
    RecordDef = [
        {data_test_goods, record_info(fields, data_test_goods)},
        {data_test_equip, record_info(fields, data_test_equip)}
    ],
    case xlsx2erl:get_sheet_data(RecordDef, FileName) of
        SheetList when is_list(SheetList) ->
            %% 转换成erlang数据, 同时检查数据结构是否正确
            %% todo 可以生成自定义数据, 可以用来优化表交叉验证数据的效率
            %% todo 默认生成record第一个字段为key的dict结构
            {SheetList1, DictList} = update_dets_convert(SheetList, [], []),
            Now = erlang:localtime(),
            dets:insert(?DETS_XLSX2ERL1(?MODULE), #xlsx2erl_excel{name = ?MODULE, excel_name = filename:basename(FileName), sheet_list = SheetList1}),
            dets:insert(?DETS_XLSX2ERL1(?MODULE), #xlsx2erl_dets{k = ?XLSX2ERL_EXCEL_UPDATE_TIME, v = Now}),
            %% todo 保存自定义数据
            lists:foreach(fun({K, V}) ->
                dets:insert(?DETS_XLSX2ERL1(?MODULE), #xlsx2erl_dets{k = ?DETS_DICT1(K), v = V})
                          end, DictList);
        _ ->
            error
    end.

update_dets_convert([], SheetList, DictList) ->
    {SheetList, DictList};
update_dets_convert([#xlsx2erl_sheet{name = data_test_goods, row_list = RowList} = H | T], SheetList, DictList) ->
    {OldRowList, OldDict} = update_dets_convert_sheet_and_dict(data_test_goods, SheetList, DictList),
    {RowList1, Dict1} = update_dets_convert_record_goods(RowList, H, record_info(fields, data_test_goods), OldRowList, OldDict),
    SheetList1 = lists:keystore(data_test_goods, #xlsx2erl_sheet.name, SheetList, H#xlsx2erl_sheet{row_list = RowList1}),
    DictList1 = lists:keystore(data_test_goods, 1, DictList, {data_test_goods, Dict1}),
    update_dets_convert(T, SheetList1, DictList1);
update_dets_convert([#xlsx2erl_sheet{name = data_test_equip, row_list = RowList} = H | T], SheetList, DictList) ->
    {OldRowList, OldDict} = update_dets_convert_sheet_and_dict(data_test_equip, SheetList, DictList),
    {RowList1, Dict1} = update_dets_convert_record_equip(RowList, H, record_info(fields, data_test_equip), OldRowList, OldDict),
    SheetList1 = lists:keystore(data_test_equip, #xlsx2erl_sheet.name, SheetList, H#xlsx2erl_sheet{row_list = RowList1}),
    DictList1 = lists:keystore(data_test_equip, 1, DictList, {data_test_equip, Dict1}),
    update_dets_convert(T, SheetList1, DictList1).

update_dets_convert_sheet_and_dict(Name, SheetList, DictList) ->
    case lists:keyfind(Name, #xlsx2erl_sheet.name, SheetList) of
        false -> OldRowList = [];
        #xlsx2erl_sheet{row_list = OldRowList} -> ok
    end,
    case lists:keyfind(Name, 1, DictList) of
        false -> OldDict = dict:new();
        {_, OldDict} -> ok
    end,
    {OldRowList, OldDict}.

update_dets_convert_record_goods([], _Sheet, _RecordDef, RowList, Dict) ->
    {RowList, Dict};
update_dets_convert_record_goods([H | T], Sheet, RecordDef, RowList, Dict) ->
    %% todo 选择转换类型
    Record1 = #data_test_goods{
        id = xlsx2erl_util:convert_int(#data_test_goods.id, RecordDef, H, Sheet),
        type = xlsx2erl_util:convert_int(#data_test_goods.type, RecordDef, H, Sheet),
        name = xlsx2erl_util:convert_bin(#data_test_goods.name, RecordDef, H, Sheet),
        price = xlsx2erl_util:convert_float(#data_test_goods.price, RecordDef, H, Sheet),
        resolve_reward = xlsx2erl_util:convert_json(#data_test_goods.resolve_reward, RecordDef, H, Sheet)
    },
    RowList1 = [H#xlsx2erl_row{record = Record1} | RowList],
    Dict1 = dict:store(element(2, Record1), Record1, Dict),
    update_dets_convert_record_goods(T, Sheet, RecordDef, RowList1, Dict1).

update_dets_convert_record_equip([], _Sheet, _RecordDef, RowList, Dict) ->
    {RowList, Dict};
update_dets_convert_record_equip([H | T], Sheet, RecordDef, RowList, Dict) ->
    %% todo 选择转换类型
    Record1 = #data_test_equip{
        id = xlsx2erl_util:convert_int(#data_test_equip.id, RecordDef, H, Sheet),
        attr = xlsx2erl_util:convert_json(#data_test_equip.attr, RecordDef, H, Sheet)
    },
    RowList1 = [H#xlsx2erl_row{record = Record1} | RowList],
    %% todo 构造数据索引
    Dict1 = dict:store(element(2, Record1), Record1, Dict),
    update_dets_convert_record_equip(T, Sheet, RecordDef, RowList1, Dict1).

%% todo 对应的获取自定义数据方法, 默认获取dict
get_goods() ->
    xlsx2erl_util:ensure_dets(?DETS_XLSX2ERL1(?MODULE)),
    [#xlsx2erl_dets{v = V}] = dets:lookup(?DETS_XLSX2ERL1(?MODULE), ?DETS_DICT1(data_test_goods)),
    V.

get_equip() ->
    xlsx2erl_util:ensure_dets(?DETS_XLSX2ERL1(?MODULE)),
    [#xlsx2erl_dets{v = V}] = dets:lookup(?DETS_XLSX2ERL1(?MODULE), ?DETS_DICT1(data_test_equip)),
    V.

compile(#xlsx2erl_cb_args{hrl_path = HrlPath} = Args) ->
    #xlsx2erl_excel{sheet_list = SheetList} = xlsx2erl_util:get_excel(?MODULE),
    xlsx2erl_util:copy_mask_body(?MODULE, HrlPath ++ "/" ++ ?XLSX2ERL_DEFAULT_HRL),
    do_compile(SheetList, Args).

do_compile([], _Args) ->
    ok;
do_compile([#xlsx2erl_sheet{name = data_test_goods, row_list = RowList} = Sheet | T], #xlsx2erl_cb_args{erl_path = ErlPath} = Args) ->
    Head =
        "-module(data_test_goods).\n\n"
    "-include(\"" ++ ?XLSX2ERL_DEFAULT_HRL ++ "\").\n\n"
    "-export([get/1]).\n\n",
    Body =
        lists:map(fun(#xlsx2erl_row{record = Record} = Row) ->
            %% todo 添加数值检查
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
                            end, [], Record#data_test_goods.resolve_reward),
            "get(" ++ xlsx2erl_util:to_iolist(Record#data_test_goods.id) ++ ") -> \n" ++
                "    #data_test_goods{\n"
                "        id = " ++ xlsx2erl_util:to_iolist(Record#data_test_goods.id) ++ ", \n" ++
                "        type = " ++ xlsx2erl_util:to_iolist(Record#data_test_goods.type) ++ ", \n" ++
                "        name = " ++ xlsx2erl_util:to_iolist(Record#data_test_goods.name) ++ ", \n" ++
                "        price = " ++ xlsx2erl_util:to_iolist(Record#data_test_goods.price) ++ ", \n" ++
                "        resolve_reward = " ++ xlsx2erl_util:to_iolist(ResolveReward) ++ "\n" ++
                "};\n"
                  end, RowList),
    Tail =
        "get(_) -> undefined.",
    ok = file:write_file(ErlPath ++ "/data_test_goods.erl", [Head, Body, Tail]),
    do_compile(T, Args);
do_compile([#xlsx2erl_sheet{name = data_test_equip, row_list = RowList} = Sheet | T], #xlsx2erl_cb_args{erl_path = ErlPath} = Args) ->
    Head =
        "-module(data_test_equip).\n\n"
    "-include(\"" ++ ?XLSX2ERL_DEFAULT_HRL ++ "\").\n\n"
    "-export([get/1]).\n\n",
    Body =
        lists:map(fun(#xlsx2erl_row{record = Record} = Row) ->
            %% 转换成 [{属性名(atom), 属性值}]
            Attr =
                try
                    [{binary_to_atom(K), V} || {K, V} <- maps:to_list(Record#data_test_equip.attr)]
                catch
                    _:_ ->
                        ?XLSX2ERL_ERROR4(Sheet, Row, "attr: ~ts", [jsx:encode(Record#data_test_equip.attr)]),
                        exit(badarg)
                end,
            [
                    "get(" ++ xlsx2erl_util:to_iolist(Record#data_test_equip.id) ++ ") ->\n" ++
                    "    #data_test_equip{"
                    "id = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.id) ++ ", " ++
                    "attr = " ++ xlsx2erl_util:to_iolist(Attr) ++
                    "};\n"
            ]
                  end, RowList),
    Tail =
        "get(_) -> undefined.",
    ok = file:write_file(ErlPath ++ "/data_test_equip.erl", [Head, Body, Tail]),
    do_compile(T, Args).

clean(#xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath}) ->
    file:delete(HrlPath ++ "/" ++ ?XLSX2ERL_DEFAULT_HRL),
    catch dets:close(?DETS_XLSX2ERL1(?MODULE)),
    file:delete(?DETS_PATH ++ "/" ++ ?MODULE_STRING),
    file:delete(ErlPath ++ "/data_test_goods.erl"),
    file:delete(ErlPath ++ "/data_test_equip.erl").
