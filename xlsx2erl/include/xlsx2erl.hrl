-define(DETS_PATH, "dets").
-define(DETS_XLSX2ERL1(Module), Module).
-define(XLSX2ERL_DETS_EXCEL_UPDATE1(Module), {Module, update_time}).
-define(XLSX2ERL_DETS_EXCEL_UPDATE2(Module, Time), #xlsx2erl_excel_update{tag = ?XLSX2ERL_DETS_EXCEL_UPDATE1(Module), time = Time}).

-define(XLSX2ERL_CALLBACK_PATH, "src/callback").

-define(XLSX2ERL_RECORD_START_MASK1(ModuleStr), "%%%%%%%%%%%" ++ ModuleStr ++ " record define start%%%%%%%%%%%%%%%%%").
-define(XLSX2ERL_RECORD_END_MASK1(ModuleStr), "%%%%%%%%%%%" ++ ModuleStr ++ " record define end%%%%%%%%%%%%%%%%%%%").

-define(XLSX2ERL_ERROR4(Sheet, Row, Format, Args),
    io:format("配错了!!!!!,工作簿 ~ts 数据表 ~ts 第 ~p 行~n" ++ Format ++ "~n",
        [Sheet#xlsx2erl_sheet.excel_name, Sheet#xlsx2erl_sheet.sheet_name, Row#xlsx2erl_row.line] ++ Args)).% 打印错误

-define(XLSX2ERL_DEFAULT_DATA_MODULE(Tag), "data_" ++ atom_to_list(Tag)).
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
    sheet_list :: [#xlsx2erl_sheet{}]
}).

-record(xlsx2erl_excel_update, {
    tag :: {update_time, Module :: atom()},
    time :: non_neg_integer()
}).

-record(xlsx2erl_callback_args, {
    export_path
}).