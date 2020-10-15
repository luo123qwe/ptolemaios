-record(row, {
    line :: integer(),
    record :: tuple()
}).

-record(sheet, {
    name :: string(),% 表名
    nth_list :: list(),% 对应excel表的列下标
    row_list :: [#row{}]
}).

-record(callback_args, {
    filename,
    export_path
}).