%% 数据状态
-define(VIRTURE_STATE_DELETE, 0).
-define(VIRTURE_STATE_INSERT, 1).
-define(VIRTURE_STATE_UPDATE, 2).

%% change
-record(virture_change, {
    key,
    type,
    record
}).

%% 一份代码相当于对应一个数据库和一个连接池
%% 所以如果需要使用多个数据库和连接池的话, 复制一份代码并且替换宏即可
%%%%%%%%%%%%%%%%%%%%%%%%%%%% virture mysql %%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(PD_VMYSQL_POOL, pd_vmysql_pool).
-define(PD_VMYSQL_CHANGE, pd_vmysql_change).
-define(PD_VMYSQL_CACHE, pd_vmysql_cache).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% field
-record(vmysql_field, {
    name :: atom(),
    type :: virture:field_type(),
    pos :: integer(),
    default :: term()
}).

%% table
-record(vmysql, {
    %% 配置项
    table :: atom(),% 数据表名和mysql一样
    private_pos_key :: integer(),% record需要预留一个给key, 联合主键避免重新构造key
    select_key :: [],% 数据库搜索全部数据用的key
    private_key :: [],% 数据的主键
    all_fields :: [#vmysql_field{}],% 所有列的定义
    record_size :: integer(),% record的定义, 因为record的字段不一定全都是数据库里面的
    init_fun :: undefined|{M :: atom(), F :: atom()},% fun((Record) -> Record1), 初始化缓存变量
    data :: dict:dict()|list(),% record底层存储类型, 如果数据量比较大的话使用dict效率更高
    %% 同步策略, 可以同时存在, 互相独立
    sync_time :: integer(),% 定时同步
    
    %% 不用管的, 小优化
    where_sql :: iolist(),
    select_sql :: iolist(),
    insert_sql :: iolist(),
    update_sql :: iolist(),
    delete_sql :: iolist()
}).



