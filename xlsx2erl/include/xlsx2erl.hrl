-define(XLSX2ERL_SPLIT, "_").

-define(XLSX2ERL_DETS_PATH, "dets").% dets路径
-define(XLSX2ERL_DETS_TABLE1(Module),
    case is_atom(Module) of
        true -> list_to_atom("xlsx_" ++ atom_to_list(Module));
        _ -> list_to_atom("xlsx_" ++ Module)
    end).% dets表名
%% dets key
-define(XLSX2ERL_DETS_KEY_SHEET1(Tag), {sheet, Tag}).% dets sheet
-define(XLSX2ERL_DETS_KEY_UPDATE1(Tag), {update, Tag}).% dets更新时间
-define(XLSX2ERL_DETS_KEY_INDEX1(Tag), {index, Tag}).% dets索引

-define(XLSX2ERL_CB_PATH, "src/callback").% callback路径
-define(XLSX2ERL_INCLUDE_PATH, "include").% include路径

%% mask字符串
-define(XLSX2ERL_MASK_START1(Tag), "%% xlsx2erl mask start " ++ Tag).
-define(XLSX2ERL_MASK_END1(Tag), "%% xlsx2erl mask end " ++ Tag).

%% sheet第一行key
-define(XLSX2ERL_KEY_SEARCH_LIMIT, 100).% 工作表搜索非XLSX2ERL_MASK_DATA最大行数
-define(XLSX2ERL_KEY_COMMENT, "COMMENT").% 工作表标识哪一行是注释
-define(XLSX2ERL_KEY_KEY, "KEY").% 工作表标识哪一行是key
-define(XLSX2ERL_KEY_DATA, "DATA").% 工作表标识哪一行是data
-define(XLSX2ERL_KEY_LIST, [?XLSX2ERL_KEY_COMMENT, ?XLSX2ERL_KEY_KEY, ?XLSX2ERL_KEY_DATA]).

%% 进程字典
%% XLSX2ERL_ERROR2用的
-define(PD_XLSX2ERL_WORKBOOK, pd_xlsx2erl_workbook).
-define(PD_XLSX2ERL_SHEET, pd_xlsx2erl_sheet).
-define(PD_XLSX2ERL_ROW, pd_xlsx2erl_row).
-define(PD_XLSX2ERL_FIELD_DEF, pd_xlsx2erl_record_def).
%% 缓存数据
-define(PD_XLSX2ERL_ZIP_FD1(Filename), {pd_xlsx2erl_zip_fd, Filename}).
-define(PD_XLSX2ERL_SHARE_STRING1(Filename), {pd_xlsx2erl_share_string, Filename}).
-define(PD_XLSX2ERL_SHEET_LIST1(Filename), {pd_xlsx2erl_sheet_list, Filename}).
-define(PD_XLSX2ERL_WORKBOOK_LIST, pd_xlsx2erl_workbook_list).

%% 错误处理
-define(XLSX2ERL_ERROR, xlsx2erl_error).
-define(XLSX2ERL_ERROR2(Format, Args), io:format(Format ++ "~n", Args), exit(?XLSX2ERL_ERROR)).
%% 打印错误信息, 精确到哪一行, 需要进程字典支持
-define(XLSX2ERL_PD_ERROR2(Format, Args),
    io:format("配错了!!!!!,工作簿 ~ts 数据表 ~ts 第 ~p 行~n" ++ Format ++ "~n",
        [get(?PD_XLSX2ERL_WORKBOOK), get(?PD_XLSX2ERL_SHEET), (get(?PD_XLSX2ERL_ROW))#xlsx2erl_row.line] ++ Args),
    exit(?XLSX2ERL_ERROR)).% 打印错误

%% 默认名字
-define(XLSX2ERL_HRL_NAME1(WorkbookName), WorkbookName).
-define(XLSX2ERL_ERL_NAME2(WorkbookName, SheetName), list_to_atom("xlsx2erl_" ++ WorkbookName ++ "_" ++ SheetName)).
-define(XLSX2ERL_RECORD_NAME2(WorkbookName, SheetName), list_to_atom("data_" ++ WorkbookName ++ "_" ++ SheetName)).

%% 转换, 方便ide提示
-define(XLSX2ERL_TO_BIN2(Index, Row), xlsx2erl_util:to_bin(Index, Row)).
-define(XLSX2ERL_TO_INT2(Index, Row), xlsx2erl_util:to_int(Index, Row)).
-define(XLSX2ERL_TO_FLOAT2(Index, Row), xlsx2erl_util:to_float(Index, Row)).
-define(XLSX2ERL_TO_TERM2(Index, Row), xlsx2erl_util:to_term(Index, Row)).
-define(XLSX2ERL_TO_JSON2(Index, Row), xlsx2erl_util:to_json(Index, Row)).

-record(xlsx2erl_raw_row, {
    line :: integer(),
    data :: [{Index :: atom(), V :: string()}]
}).

-record(xlsx2erl_row, {
    line :: integer(),
    data :: Record :: tuple()
}).

-record(xlsx2erl_sheet, {
    %% excel数据
    name :: atom(),% 表对应的名字
    full_name :: string(),% 全名
    workbook_name :: atom(),
    filename :: string(),% 全名
    module :: atom(),% 对应的回调模块
    id :: string(),% r:id
    taget :: string(),% 对应sheet的xml
    data :: map()% {?XLSX2ERL_KEY_XXX => [#xlsx2erl_row{}|#xlsx2erl_raw_row{}]}
}).

-record(xlsx2erl_workbook, {
    name :: atom(),% 工作簿对应的名字
    file_name :: string()% 全名
}).

-record(xlsx2erl_cb_args, {
    erl_path,
    hrl_path
}).