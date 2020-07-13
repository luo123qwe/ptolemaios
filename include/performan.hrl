-record(performan_struct, {
    size,
    %% 对ets有影响, {N,N,N....}
    record_size,
    build_times,
    lookup_times,

    %% key不变
    update_times,

    %% key改变
    delete_insert_times,% 删除+插入的更新方式
    %% 单独测删除似乎没什么意义就不写了

    fold_times,% 遍历所有数据, to_list

    %% 范围搜索
    range :: {Min :: integer(), Max :: integer(), Times :: integer()},
    sub :: {Start :: integer(), Len :: integer(), Times :: integer()}
}).