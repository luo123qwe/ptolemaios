-module(xlsx2erl_test_equip).

-behaviour(xlsx2erl_callback).


-include("xlsx2erl.hrl").
-include("xlsx2erl_test_equip.hrl").

%% workbook和sheet的名字
-define(PRIV_WORKBOOK_NAME, "test").
-define(PRIV_SHEET_NAME, "equip").
%% dets
-define(PRIV_DETS, ?XLSX2ERL_DETS_TABLE1(?PRIV_WORKBOOK_NAME)).
-define(PRIV_DETS_SHEET, list_to_atom(?PRIV_SHEET_NAME)).
-define(PRIV_DETS_INDEX, {index, ?MODULE}).
%% todo 生成的文件名字, hrl复制的mask
-define(PRIV_ERL_FILE, "data_test_equip.erl").
-define(PRIV_HRL_FILE, "test.hrl").
-define(PRIV_MASK_TAG, "data_test_equip").

%% todo compile_body用的arg
-record(priv_arg, {
    body = [],% 生成内容
    keys = #{}% 全部key, 检查key是否重复
}).

-export([get_index/0]).

-export([update_dets/2, compile/1, clean/1]).

%% 字典数据, 用于报错
init_pd(Sheet) ->
    put(?PD_XLSX2ERL_SHEET, Sheet),
    put(?PD_XLSX2ERL_FIELD_DEF, record_info(fields, data_test_equip)).

clean_pd() ->
    erase(?PD_XLSX2ERL_SHEET),
    erase(?PD_XLSX2ERL_FIELD_DEF),
    erase(?PD_XLSX2ERL_ROW).

update_dets(Sheet, RawData) ->
    init_pd(Sheet),
    
    RowList = xlsx2erl:sheet_data_to_record(RawData, data_test_equip, record_info(fields, data_test_equip)),
    %% todo 可以生成自定义数据索引, 用来优化表交叉验证数据的效率
    %% todo 默认生成record第一个字段为key的map结构
    {RowList1, Index} = update_dets_convert_record(RowList, [], #{}),
    dets:insert(?PRIV_DETS, {?XLSX2ERL_DETS_KEY_SHEET1(?PRIV_DETS_SHEET), Sheet}),
    dets:insert(?PRIV_DETS, {?XLSX2ERL_DETS_KEY_DATA1(?PRIV_DETS_SHEET), RowList1}),
    %% 保存自定义数据
    dets:insert(?PRIV_DETS, {?XLSX2ERL_DETS_KEY_INDEX1(?PRIV_DETS_SHEET), Index}),
    
    clean_pd().

update_dets_convert_record([], RowList, Index) ->
    {RowList, Index};
update_dets_convert_record([H | T], RowList, Index) ->
    put(?PD_XLSX2ERL_ROW, H),
    %% todo 选择转换类型
    Record1 = #data_test_equip{
        id = ?XLSX2ERL_TO_INT2(#data_test_equip.id, H),
        attr1 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr1, H),
        attr2 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr2, H),
        attr3 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr3, H),
        attr4 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr4, H),
        attr5 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr5, H),
        attr6 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr6, H),
        attr7 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr7, H),
        attr8 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr8, H),
        attr9 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr9, H),
        attr10 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr10, H),
        attr11 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr11, H),
        attr12 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr12, H),
        attr13 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr13, H),
        attr14 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr14, H),
        attr15 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr15, H),
        attr16 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr16, H),
        attr17 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr17, H),
        attr18 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr18, H),
        attr19 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr19, H),
        attr20 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr20, H),
        attr21 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr21, H),
        attr22 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr22, H),
        attr23 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr23, H),
        attr24 = ?XLSX2ERL_TO_JSON2(#data_test_equip.attr24, H)
    },
    RowList1 = [H#xlsx2erl_row{record = Record1} | RowList],
    %% todo 构造数据索引
    Index1 = Index#{Record1#data_test_equip.id => Record1},
    update_dets_convert_record(T, RowList1, Index1).

%% todo 对应的获取自定义索引
get_index() ->
    xlsx2erl_util:ensure_dets(?PRIV_DETS),
    K = ?XLSX2ERL_DETS_KEY_INDEX1(?PRIV_DETS_SHEET),
    [{_, V}] = dets:lookup(?PRIV_DETS, K),
    V.

compile(#xlsx2erl_cb_args{hrl_path = HrlPath, erl_path = ErlPath}) ->
    [{_, Sheet}] = dets:lookup(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_SHEET1(?PRIV_DETS_SHEET)),
    [{_, RowList}] = dets:lookup(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_DATA1(?PRIV_DETS_SHEET)),
    init_pd(Sheet),
    
    %% todo 如果不需要复制hrl定义可以删除这句
    xlsx2erl_util:copy_mask_body(?MODULE, ?PRIV_MASK_TAG, HrlPath ++ "/" ++ ?PRIV_HRL_FILE),
    
    %% todo 构造文件内容, 如果生成多个函数, priv_arg多定义几个参数即可
    Head =
        "-module(" ++ filename:rootname(?PRIV_ERL_FILE) ++ ").\n\n"
    "-include(\"plm_lib.hrl\").\n"
    "-include(\"" ++ ?PRIV_HRL_FILE ++ "\").\n\n"
    "-export([get/1, get/2]).\n\n"
    "get(Key) -> get(Key, true).\n\n",
    #priv_arg{body = Body} = compile_row_list(RowList, #priv_arg{}),
    Tail =
        "get(Key, _) ->\n"
        "    ?LOG_ERROR(\"undefined ~w in " ++ filename:rootname(?PRIV_ERL_FILE) ++ "\", [Key]),\n"
    "    undefined.",
    file:make_dir(ErlPath ++ "/" ++ ?PRIV_WORKBOOK_NAME),
    File = ErlPath ++ "/" ++ ?PRIV_WORKBOOK_NAME ++ "/" ++ ?PRIV_ERL_FILE,
    ok = file:write_file(File, [Head, Body, Tail]),
    
    clean_pd().

compile_row_list([], Arg) -> Arg;
compile_row_list([H | T], Arg) ->
    %% 再套一层, 提示报错
    put(?PD_XLSX2ERL_ROW, H),
    case catch compile_row(H, Arg) of
        {'EXIT', ?XLSX2ERL_ERROR} ->% 已知错误
            exit(?XLSX2ERL_ERROR);
        #priv_arg{} = Arg1 ->
            compile_row_list(T, Arg1);
        Error ->
            ?XLSX2ERL_PD_ERROR2("unknow error ~w", [Error]),
            exit(?XLSX2ERL_ERROR)
    end.

compile_row(#xlsx2erl_row{record = Record}, #priv_arg{body = Body, keys = Keys}) ->
    %% todo 添加数值检查
    %% todo 是否重复key
    Key = Record#data_test_equip.id,
    ?DO_IF(maps:is_key(Key, Keys), ?XLSX2ERL_PD_ERROR2("key ~w 重复了", [Key])),
    %% todo 添加数值转换
    Record1 = Record#data_test_equip{},
    BodyRow = compile_body(Record1),
    Body1 = [BodyRow | Body],
    Keys1 = Keys#{Key => 1},
    #priv_arg{body = Body1, keys = Keys1}.

%% todo 构造文本
compile_body(Record) ->
    "get(" ++ xlsx2erl_util:to_iolist(Record#data_test_equip.id) ++ ", _) -> \n" ++
        "    #data_test_equip{\n"
        "        id = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.id) ++ ", \n" ++
        "        attr1 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr1) ++ ", \n" ++
        "        attr2 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr2) ++ ", \n" ++
        "        attr3 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr3) ++ ", \n" ++
        "        attr4 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr4) ++ ", \n" ++
        "        attr5 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr5) ++ ", \n" ++
        "        attr6 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr6) ++ ", \n" ++
        "        attr7 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr7) ++ ", \n" ++
        "        attr8 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr8) ++ ", \n" ++
        "        attr9 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr9) ++ ", \n" ++
        "        attr10 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr10) ++ ", \n" ++
        "        attr11 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr11) ++ ", \n" ++
        "        attr12 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr12) ++ ", \n" ++
        "        attr13 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr13) ++ ", \n" ++
        "        attr14 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr14) ++ ", \n" ++
        "        attr15 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr15) ++ ", \n" ++
        "        attr16 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr16) ++ ", \n" ++
        "        attr17 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr17) ++ ", \n" ++
        "        attr18 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr18) ++ ", \n" ++
        "        attr19 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr19) ++ ", \n" ++
        "        attr20 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr20) ++ ", \n" ++
        "        attr21 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr21) ++ ", \n" ++
        "        attr22 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr22) ++ ", \n" ++
        "        attr23 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr23) ++ ", \n" ++
        "        attr24 = " ++ xlsx2erl_util:to_iolist(Record#data_test_equip.attr24) ++ "\n" ++
        "    };\n".

clean(#xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath}) ->
    %% 先删dets
    xlsx2erl_util:ensure_dets(?PRIV_DETS),
    dets:delete(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_SHEET1(?PRIV_DETS_SHEET)),
    dets:delete(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_DATA1(?PRIV_DETS_SHEET)),
    dets:delete(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_INDEX1(?PRIV_DETS_SHEET)),
    %% 再删文件
    %% todo 如果不需要复制hrl定义可以删除这句
    xlsx2erl_util:delete_mask_body(?PRIV_MASK_TAG, HrlPath ++ "/" ++ ?PRIV_HRL_FILE),
    file:delete(ErlPath ++ "/" ++ ?PRIV_WORKBOOK_NAME ++ "/" ++ ?PRIV_ERL_FILE).
