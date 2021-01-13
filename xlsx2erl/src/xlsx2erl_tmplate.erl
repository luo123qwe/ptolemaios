%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xlsx2erl_tmplate).
-author("dominic").

-include("xlsx2erl.hrl").


%% API
-export([make/2, make/4, make_hrl/2, make_hrl/3]).

%% @equiv make(Sheet, default)
make(#xlsx2erl_sheet{} = Sheet, Data) ->
    make(Sheet, Data, default, default);
make(Filename, SheetName) ->
    make(Filename, SheetName, default, default).

%% @doc 生成模板
make(#xlsx2erl_sheet{} = Sheet, Data, ErlPath, HrlPath) ->
    #xlsx2erl_sheet{name = SheetNameAtom, workbook_name = WorkbookNameAtom} = Sheet,
    WorkbookName = atom_to_list(WorkbookNameAtom),
    SheetName = atom_to_list(SheetNameAtom),
    ModuleStr = ?XLSX2ERL_ERL_NAME2(WorkbookName, SheetName),
    RecordNameStr = ?XLSX2ERL_RECORD_NAME2(WorkbookName, SheetName),
    [#xlsx2erl_raw_row{data = KeyList} | _] = maps:get(?XLSX2ERL_KEY_KEY, Data),
    {_, FirstKey} = hd(KeyList),
    Head =
        "-module(" ++ ModuleStr ++ ").\n\n"
    "-behaviour(xlsx2erl_callback).\n\n"
    "-include(\"util.hrl\").\n"
    "-include(\"xlsx2erl.hrl\").\n"
    "-include(\"" ++ ModuleStr ++ ".hrl\").\n\n"
    "%% workbook和sheet的名字\n"
    "-define(PRIV_WORKBOOK_NAME, \"" ++ WorkbookName ++ "\").\n"
    "-define(PRIV_SHEET_NAME, \"" ++ SheetName ++ "\").\n"
    "%% dets\n"
    "-define(PRIV_DETS, ?XLSX2ERL_DETS_TABLE1(?PRIV_WORKBOOK_NAME)).\n"
    "-define(PRIV_DETS_SHEET, list_to_atom(?PRIV_SHEET_NAME)).\n"
    "-define(PRIV_DETS_INDEX, {index, ?MODULE}).\n"
    "%% todo 生成的文件名字, hrl复制的mask\n"
    "-define(PRIV_ERL_FILE, \"" ++ RecordNameStr ++ ".erl\").\n"
    "-define(PRIV_HRL_FILE, \"" ++ ?XLSX2ERL_HRL_NAME1(WorkbookName) ++ ".hrl\").\n"
    "-define(PRIV_MASK_TAG, \"" ++ RecordNameStr ++ "\").\n\n"
    "%% todo compile_body用的arg\n"
    "-record(priv_arg, {\n"
    "    body = [],% 生成内容\n"
    "    keys = #{}% 全部key, 检查key是否重复\n"
    "}).\n\n"
    "",
    Export =
        "-export([get_index/0]).\n\n"
        "-export([update_dets/2, compile/1, clean/1]).\n\n",
    Pd =
        "%% 字典数据, 用于报错\n"
        "init_pd(Sheet) ->\n"
        "    put(?PD_XLSX2ERL_SHEET, Sheet),\n"
        "    put(?PD_XLSX2ERL_FIELD_DEF, record_info(fields, " ++ RecordNameStr ++ ")).\n\n"
    "clean_pd() ->\n"
    "    erase(?PD_XLSX2ERL_SHEET),\n"
    "    erase(?PD_XLSX2ERL_FIELD_DEF),\n"
    "    erase(?PD_XLSX2ERL_ROW).\n\n"
    "",
    UpdateDets =
        "update_dets(Sheet, RawData) ->\n"
        "    init_pd(Sheet),\n"
        "\n"
        "    RowList = xlsx2erl:sheet_data_to_record(RawData, " ++ RecordNameStr ++ ", record_info(fields, " ++ RecordNameStr ++ ")),\n"
    "    %% todo 可以生成自定义数据索引, 用来优化表交叉验证数据的效率\n"
    "    %% todo 默认生成record第一个字段为key的map结构\n"
    "    {RowList1, Index} = update_dets_convert_record(RowList, [], #{}),\n"
    "    dets:insert(?PRIV_DETS, {?XLSX2ERL_DETS_KEY_SHEET1(?PRIV_DETS_SHEET), Sheet}),\n"
    "    dets:insert(?PRIV_DETS, {?XLSX2ERL_DETS_KEY_DATA1(?PRIV_DETS_SHEET), RowList1}),\n"
    "    %% 保存自定义数据\n"
    "    dets:insert(?PRIV_DETS, {?XLSX2ERL_DETS_KEY_INDEX1(?PRIV_DETS_SHEET), Index}),\n"
    "\n"
    "    clean_pd().\n\n"
    "update_dets_convert_record([], RowList, Index) ->\n"
    "    {RowList, Index};\n"
    "update_dets_convert_record([H | T], RowList, Index) ->\n"
    "    put(?PD_XLSX2ERL_ROW, H),\n"
    "    %% todo 选择转换类型\n"
    "    Record1 = #" ++ RecordNameStr ++ "{\n" ++
        string:join([
                "        " ++ Field ++ " = ?XLSX2ERL_TO_(#" ++ RecordNameStr ++ "." ++ Field ++ ", H)"
            || {_, Field} <- KeyList], ",\n") ++ "\n"
    "    },\n"
    "    RowList1 = [H#xlsx2erl_row{record = Record1} | RowList],\n"
    "    %% todo 构造数据索引\n"
    "    Index1 = Index#{Record1#" ++ RecordNameStr ++ ".id => Record1},\n"
    "    update_dets_convert_record(T, RowList1, Index1).\n\n"
    "",
    Index =
        "%% todo 对应的获取自定义索引\n" ++
        "get_index() ->\n"
        "    xlsx2erl_util:ensure_dets(?PRIV_DETS),\n"
        "    K = ?XLSX2ERL_DETS_KEY_INDEX1(?PRIV_DETS_SHEET),\n"
        "    [{_, V}] = dets:lookup(?PRIV_DETS, K),\n"
        "    V.\n\n",
    Compile =
        "compile(#xlsx2erl_cb_args{hrl_path = HrlPath, erl_path = ErlPath}) ->\n"
        "    [{_, Sheet}] = dets:lookup(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_SHEET1(?PRIV_DETS_SHEET)),\n"
        "    [{_, RowList}] = dets:lookup(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_DATA1(?PRIV_DETS_SHEET)),\n"
        "    init_pd(Sheet),\n"
        "\n"
        "    %% todo 如果不需要复制hrl定义可以删除这句\n"
        "    xlsx2erl_util:copy_mask_body(?MODULE, ?PRIV_MASK_TAG, HrlPath ++ \"/\" ++ ?PRIV_HRL_FILE),\n"
        "\n"
        "    %% todo 构造文件内容, 如果生成多个函数, priv_arg多定义几个参数即可\n"
        "    Head =\n"
        "        \"-module(\" ++ filename:rootname(?PRIV_ERL_FILE) ++ \").\\n\\n\"\n"
        "    \"-include(\\\"util.hrl\\\").\\n\"\n"
        "    \"-include(\\\"\" ++ ?PRIV_HRL_FILE ++ \"\\\").\\n\\n\"\n"
        "    \"-export([get/1, get/2]).\\n\\n\",\n"
        "    \"get(Key) -> get(Key, true).\\n\\n\",\n"
        "    #priv_arg{body = Body} = compile_row_list(RowList, #priv_arg{}),\n"
        "    Tail =\n"
        "        \"get(Key, _) ->\\n\"\n"
        "        \"    ?LOG_ERROR(\\\"undefined ~w in \" ++ filename:rootname(?PRIV_ERL_FILE) ++ \"\\\", [Key]),\\n\"\n"
        "    \"    undefined.\",\n"
        "    file:make_dir(ErlPath ++ \"/\" ++ ?PRIV_WORKBOOK_NAME),\n"
        "    File = ErlPath ++ \"/\" ++ ?PRIV_WORKBOOK_NAME ++ \"/\" ++ ?PRIV_ERL_FILE,\n"
        "    ok = file:write_file(File, [Head, Body, Tail]),\n"
        "\n"
        "    clean_pd().\n\n"
        "compile_row_list([], Arg) -> Arg;\n"
        "compile_row_list([H | T], Arg) ->\n"
        "    %% 再套一层, 提示报错\n"
        "    put(?PD_XLSX2ERL_ROW, H),\n"
        "    case catch compile_row(H, Arg) of\n"
        "        {'EXIT', ?XLSX2ERL_ERROR} ->% 已知错误\n"
        "            exit(?XLSX2ERL_ERROR);\n"
        "        #priv_arg{} = Arg1 ->\n"
        "            compile_row_list(T, Arg1);\n"
        "        Error ->\n"
        "            ?XLSX2ERL_PD_ERROR2(\"unknow error ~w~n\", [Error]),\n"
        "            exit(?XLSX2ERL_ERROR)\n"
        "    end.\n\n"
        "compile_row(#xlsx2erl_row{record = Record}, #priv_arg{body = Body, keys = Keys}) ->\n"
        "    %% todo 添加数值检查\n"
        "    %% todo 是否重复key\n"
        "    Key = Record#" ++ RecordNameStr ++ "." ++ FirstKey ++ ",\n"
    "    ?DO_IF(maps:is_key(Key, Keys), ?XLSX2ERL_PD_ERROR2(\"key ~w 重复了\", [Key])),\n"
    "    %% todo 添加数值转换\n"
    "    Record1 = Record#" ++ RecordNameStr ++ "{},\n"
    "    BodyRow = compile_body(Record1),\n"
    "    Body1 = [BodyRow | Body],\n"
    "    Keys1 = Keys#{Key => 1},\n"
    "    #priv_arg{body = Body1, keys = Keys1}.\n\n"
    "%% todo 构造文本\n"
    "compile_body(Record) ->\n"
    "    \"get(\" ++ xlsx2erl_util:to_iolist(Record#" ++ RecordNameStr ++ "." ++ FirstKey ++ ") ++ \") -> \\n\" ++\n"
    "        \"    #" ++ RecordNameStr ++ "{\\n\"\n" ++
        string:join([
                "        \"        " ++ Field ++ " = \" ++ xlsx2erl_util:to_iolist(Record#" ++ RecordNameStr ++ "." ++ Field ++ ") ++"
            || {_, Field} <- KeyList], " \", \\n\" ++\n") ++ " \"\\n\" ++\n"
    "        \"    };\\n\".\n\n"
    "",
    Clean =
        "clean(#xlsx2erl_cb_args{erl_path = ErlPath, hrl_path = HrlPath}) ->\n"
        "    %% 先删dets\n"
        "    xlsx2erl_util:ensure_dets(?PRIV_DETS),\n"
        "    dets:delete(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_SHEET1(?PRIV_DETS_SHEET)),\n"
        "    dets:delete(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_DATA1(?PRIV_DETS_SHEET)),\n"
        "    dets:delete(?PRIV_DETS, ?XLSX2ERL_DETS_KEY_INDEX1(?PRIV_DETS_SHEET)),\n"
        "    %% 再删文件\n"
        "    %% todo 如果不需要复制hrl定义可以删除这句\n"
        "    xlsx2erl_util:delete_mask_body(?PRIV_MASK_TAG, HrlPath ++ \"/\" ++ ?PRIV_HRL_FILE),\n"
        "    file:delete(ErlPath ++ \"/\" ++ ?PRIV_WORKBOOK_NAME ++ \"/\" ++ ?PRIV_ERL_FILE).\n"
        "",
    ErlFile =
        case ErlPath of
            default ->
                Dir = ?XLSX2ERL_CB_PATH ++ "/" ++ WorkbookName,
                file:make_dir(Dir),
                Dir ++ "/" ++ ModuleStr ++ ".erl";
            _ ->
                case filelib:is_dir(ErlPath) of
                    true ->
                        Dir = ErlPath ++ "/" ++ WorkbookName,
                        file:make_dir(Dir),
                        Dir ++ "/" ++ ModuleStr ++ ".erl";
                    _ ->
                        ?XLSX2ERL_ERROR2("~ts not dir!", [ErlPath])
                end
        end,
    ?LOG_NOTICE("write template file " ++ ErlFile),
    ok = file:write_file(ErlFile, unicode:characters_to_binary([Head, Export, Pd, UpdateDets, Index, Compile, Clean])),
    make_hrl(Sheet, Data, HrlPath);
make(Filename, SheetName, ErlPath, HrlPath) ->
    SheetWithDataList = xlsx2erl_reader:sheets_with_data(Filename, [{?XLSX2ERL_KEY_KEY, 1}, {?XLSX2ERL_KEY_COMMENT, 1}], ?XLSX2ERL_KEY_SEARCH_LIMIT),
    SheetNameAtom = list_to_atom(SheetName),
    case lists:keyfind(SheetNameAtom, #xlsx2erl_sheet_with_data.name, SheetWithDataList) of
        false -> ?XLSX2ERL_ERROR2("no sheet ~ts in ~ts", [SheetName, Filename]);
        #xlsx2erl_sheet_with_data{sheet = Sheet, data = Data} -> make(Sheet, Data, ErlPath, HrlPath)
    end.

make_hrl(#xlsx2erl_sheet{} = Sheet, Data) ->
    make_hrl(Sheet, Data, default);
make_hrl(Filename, SheetName) ->
    make_hrl(Filename, SheetName, default).

make_hrl(#xlsx2erl_sheet{} = Sheet, Data, HrlPath) ->
    #xlsx2erl_sheet{
        name = SheetNameAtom, full_name = SheetFullName,
        workbook_name = WorkbookNameAtom
    } = Sheet,
    WorkbookName = atom_to_list(WorkbookNameAtom),
    SheetName = atom_to_list(SheetNameAtom),
    ModuleStr = ?XLSX2ERL_ERL_NAME2(WorkbookName, SheetName),
    RecordNameStr = ?XLSX2ERL_RECORD_NAME2(WorkbookName, SheetName),
    [#xlsx2erl_raw_row{data = KeyList} | _] = maps:get(?XLSX2ERL_KEY_KEY, Data),
    [#xlsx2erl_raw_row{data = CommentList} | _] = maps:get(?XLSX2ERL_KEY_COMMENT, Data, [#xlsx2erl_raw_row{data = []}]),
    [SheetComment, _] = string:split(SheetFullName, ?XLSX2ERL_SPLIT),
    
    Head = ?XLSX2ERL_MASK_START1(RecordNameStr) ++ "\n",
    Body =
        %% 转换成unicode的0-255编码
        "%% " ++ [unicode:characters_to_binary(SheetComment)] ++ "\n"
    "-record(" ++ RecordNameStr ++ ", {\n"
        ++ make_field_comment_str(KeyList, CommentList) ++
        "}).\n"
        "",
    Tail = ?XLSX2ERL_MASK_END1(RecordNameStr) ++ "\n",
    HrlFile =
        case HrlPath of
            default ->
                ?XLSX2ERL_INCLUDE_PATH ++ "/" ++ ModuleStr ++ ".hrl";
            _ ->
                case filelib:is_dir(HrlPath) of
                    true ->
                        HrlPath ++ "/" ++ ModuleStr ++ ".hrl";
                    _ ->
                        ?XLSX2ERL_ERROR2("~ts not dir!", [HrlPath])
                end
        end,
    ?LOG_NOTICE("write template file " ++ HrlFile),
    ok = file:write_file(HrlFile, [Head, Body, Tail]);
make_hrl(Filename, SheetName, HrlPath) ->
    SheetWithDataList = xlsx2erl_reader:sheets_with_data(Filename, [{?XLSX2ERL_KEY_KEY, 1}, {?XLSX2ERL_KEY_COMMENT, 1}], ?XLSX2ERL_KEY_SEARCH_LIMIT),
    SheetNameAtom = list_to_atom(SheetName),
    case lists:keyfind(SheetNameAtom, #xlsx2erl_sheet_with_data.name, SheetWithDataList) of
        false -> ?XLSX2ERL_ERROR2("no sheet ~ts in ~ts", [SheetName, Filename]);
        #xlsx2erl_sheet_with_data{sheet = Sheet, data = Data} -> make_hrl(Sheet, Data, HrlPath)
    end.

make_field_comment_str([{Index, Key} | T], CommentList) ->
    case lists:keyfind(Index, 1, CommentList) of
        false -> Comment = "";
        {_, Comment} -> ok
    end,
    case T of
        [] -> ["    ", Key, "% ", Comment, "\n"];
        _ -> ["    ", Key, ",% ", Comment, "\n" | make_field_comment_str(T, CommentList)]
    end.
    
    
    