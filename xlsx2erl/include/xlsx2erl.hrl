-define(DETS_XLSX2ERL, dets_xlsx2erl).
-define(XLSX2ERL_DETS_EXCEL_UPDATE1(Module), {Module, update_time}).
-define(XLSX2ERL_DETS_EXCEL_UPDATE2(Module, Time), #excel_update{tag = ?XLSX2ERL_DETS_EXCEL_UPDATE1(Module), time = Time}).

-define(XLSX2ERL_RECORD_START_MASK1(ModuleStr), "%%%%%%%%%%%" ++ ModuleStr ++ " record define start%%%%%%%%%%%%%%%%%%%").
-define(XLSX2ERL_RECORD_END_MASK1(ModuleStr), "%%%%%%%%%%%" ++ ModuleStr ++ " record define end%%%%%%%%%%%%%%%%%%%").

-record(row, {
    line :: integer(),
    record :: tuple()
}).

-record(sheet, {
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