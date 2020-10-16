-define(XLSX2ERL_RECORD_START_MASK, "%%%%%%%%%%%" ++ ?MODULE_STRING ++ " record define start%%%%%%%%%%%%%%%%%%%").
-define(XLSX2ERL_RECORD_START_MASK(ModuleStr), "%%%%%%%%%%%" ++ ModuleStr ++ " record define start%%%%%%%%%%%%%%%%%%%").
-define(XLSX2ERL_RECORD_END_MASK, "%%%%%%%%%%%" ++ ?MODULE_STRING ++ " record define end%%%%%%%%%%%%%%%%%%%").
-define(XLSX2ERL_RECORD_END_MASK(ModuleStr), "%%%%%%%%%%%" ++ ModuleStr ++ " record define end%%%%%%%%%%%%%%%%%%%").

-record(row, {
    line :: integer(),
    record :: tuple()
}).

-record(sheet, {
    record_name :: atom(),% 表对应的record名字
    nth_list :: list(),% 对应excel表的列下标
    row_list :: [#row{}]
}).

-record(callback_args, {
    filename,
    export_path
}).
