%% 一个数据块的长度
%% 当数据量=<16, 使用list存储
%% 当数据量>16, 使用dict存储, 且不会因为数量减少换成list
-define(VIRUTE_CHUNK_SIZE, 16).
%% 数据状态
-define(VIRTURE_STATE_DELETE, 0).
-define(VIRTURE_STATE_INSERT, 1).


%% field
-record(virture_field, {
    name :: atom(),
    type :: virture:field_type(),
    pos :: integer(),
    default :: term()
}).

%% table
-record(virture, {
    %% 配置项
    table :: atom(),% 数据表名和mysql一样
    select_key :: [],% 数据库搜索全部数据用的key, 默认player_id
    private_key :: [],% 数据的主键
    all_fields :: [#virture_field{}],% 所有行的定义
    record_size :: integer(),% record的定义
    init_fun :: undefined|{M :: atom(), F :: atom()},% fun((Record) -> Record1)
    data :: dict:dict()|list(),% 数据record存储类型
    %% 不用管的
    where_sql :: iolist(),
    select_sql :: iolist(),
    insert_sql :: iolist(),
    update_sql :: iolist(),
    delete_sql :: iolist()
}).

%% change
-record(virture_change, {
    key,
    type,
    record
}).



