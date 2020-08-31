%% 一般情况增删改, 总次数 = Size * XXXTimes

-record(performance_fractal_tree, {
    size,
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

-define(PD_PERFORMANCE_HASH_TUPLE_RAND_LIST, pd_performance_hash_tuple_rand_list).

-record(performance_hash_tuple, {
    size,
    build_times,
    lookup_times,
    store_times,
    fold_times% 遍历所有数据, to_list
}).

-record(performance_element, {
    size1,% 两个不同size跑同一份代码
    size2,
    lookup_times,
    store_times
}).

-record(performance_virture, {
    size,
    lookup_times,
    store_times,
    fold_times
}).