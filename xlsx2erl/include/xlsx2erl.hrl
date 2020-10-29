-define(DETS_XLSX2ERL, dets_xlsx2erl).
-define(XLSX2ERL_DETS_EXCEL_UPDATE1(Module), {Module, update_time}).
-define(XLSX2ERL_DETS_EXCEL_UPDATE2(Module, Time), #excel_update{tag = ?XLSX2ERL_DETS_EXCEL_UPDATE1(Module), time = Time}).

-define(PD_XLSX2ERL_ROW, pd_xlsx2erl_row).

-define(XLSX2ERL_RECORD_START_MASK1(ModuleStr), "%%%%%%%%%%%" ++ ModuleStr ++ " record define start%%%%%%%%%%%%%%%%%%%").
-define(XLSX2ERL_RECORD_END_MASK1(ModuleStr), "%%%%%%%%%%%" ++ ModuleStr ++ " record define end%%%%%%%%%%%%%%%%%%%").

-define(XLSX2ERL_ERROR(Sheet, Row, Format, Args), io:format("配错了!!!!!,工作簿 ~ts 数据表 ~ts 第 ~p 行~n" ++ Format, [Sheet#sheet.excel_name, Sheet#sheet.sheet_name, Row#row.line] ++ Args)).% 打印错误

-record(row, {
    line :: integer(),
    record :: tuple()
}).

-record(sheet, {
    excel_name :: string(),% 全名
    sheet_name :: string(),% 全名
    name :: atom(),% 表对应的名字
    nth_list :: list(),% record字段对应excel表的列下标
    row_list :: [#row{}]
}).

-record(excel, {
    name :: atom(),% 工作簿对应的名字
    sheet_list :: [#sheet{}]
}).

-record(excel_update, {
    tag :: {update_time, Module :: atom()},
    time :: non_neg_integer()
}).

-record(callback_args, {
    export_path
}).