-define(DETS_PATH, "dets").% dets路径
-define(DETS_XLSX2ERL1(Module), Module).% dets表名
%% dets key
-define(XLSX2ERL_EXCEL_UPDATE_TIME, prim_excel_update_time).

-define(XLSX2ERL_CALLBACK_PATH, "src/callback").% callback路径
-define(XLSX2ERL_INCLUDE_PATH, "include").% include路径

%% mask字符串
-define(XLSX2ERL_RECORD_START_MASK1(ModuleStr), "%%%%%%%%%%%" ++ ModuleStr ++ " record define start%%%%%%%%%%%%%%%%%").
-define(XLSX2ERL_RECORD_END_MASK1(ModuleStr), "%%%%%%%%%%%" ++ ModuleStr ++ " record define end%%%%%%%%%%%%%%%%%%%").

-define(XLSX2ERL_KEY_MASK, "KEY").% 工作表标识哪一行是key
-define(XLSX2ERL_DATA_MASK, "DATA").% 工作表标识哪一行是data

%% 打印错误信息, 精确到哪一行
-define(XLSX2ERL_ERROR4(Sheet, Row, Format, Args),
    io:format("配错了!!!!!,工作簿 ~ts 数据表 ~ts 第 ~p 行~n" ++ Format ++ "~n",
        [Sheet#xlsx2erl_sheet.excel_name, Sheet#xlsx2erl_sheet.sheet_name, Row#xlsx2erl_row.line] ++ Args)).% 打印错误

%% 默认名字
-define(XLSX2ERL_DEFAULT_HRL, (?MODULE_STRING -- "xlsx2erl_") ++ ".hrl").

-record(xlsx2erl_row, {
    line :: integer(),
    record :: tuple()
}).

-record(xlsx2erl_sheet, {
    excel_name :: string(),% 全名
    sheet_name :: string(),% 全名
    name :: atom(),% 表对应的名字
    nth_list :: list(),% record字段对应excel表的列下标
    row_list :: [#xlsx2erl_row{}]
}).

-record(xlsx2erl_excel, {
    name :: atom(),% 工作簿对应的名字
    excel_name :: string(),% 全名
    sheet_list :: [#xlsx2erl_sheet{}]
}).

%% dets 杂项
-record(xlsx2erl_dets, {
    k,
    v
}).

-record(xlsx2erl_cb_args, {
    erl_path,
    hrl_path
}).