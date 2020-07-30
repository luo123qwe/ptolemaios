%% 数据状态
-define(VIRTURE_STATE_DELETE, 0).
-define(VIRTURE_STATE_REPLACE, 1).
-define(VIRTURE_STATE_NOT_CHANGE, 2).

%% 带着size的列表结构
-define(VIRTURE_LIST, {0, []}).
-define(VIRTURE_LIST(Size, List), {Size, List}).

%% JSON结构定义
%% 支持
%% 元组 => obj, {1,2} => {"a":1,"b":2}
%% 列表 => 列表, [1,a] => [1, "a"]
%% 元组列表 => 对象列表, [{1,2}] => [{"a":1,"b":2}]
-define(VIRTURE_JSON_LIST, json_list).% 列表, ["a", 1, VIRTURE_JSON_嵌套用]
-define(VIRTURE_JSON_LIST(Name), {json_list, Name}).% 嵌套用
-define(VIRTURE_JSON_OBJ(NameList), {json_obj, NameList}).% 元组, {"a":1, "b":1, VIRTURE_JSON_嵌套用}
-define(VIRTURE_JSON_OBJ(Name, NameList), {json_obj, Name, NameList}).% 嵌套用
-define(VIRTURE_JSON_OBJ_LIST(NameList), {json_obj_list, NameList}).% 列表元组, [{"a":1, "b":1}]
-define(VIRTURE_JSON_OBJ_LIST(Name, NameList), {json_obj_list, Name, NameList}).% 嵌套用

%% 数据类型
-define(VIRTURE_INT32, int32).
-define(VIRTURE_UINT32, uint32).
-define(VIRTURE_INT64, int64).
-define(VIRTURE_UINT64, uint64).
-define(VIRTURE_FLOAT, float).
-define(VIRTURE_STRING, string).
-define(VIRTURE_TO_STRING, to_string).
-define(VIRTURE_BINARY, binary).
-define(VIRTURE_TO_BINARY, to_binary).
-define(VIRTURE_TO_JSON(Struct), {to_json, Struct}).


%% 一份代码相当于对应一个数据库和一个连接池
%% 所以如果需要使用多个数据库和连接池的话, 复制一份代码并且替换宏即可
%%%%%%%%%%%%%%%%%%%%%%%%%%%% virture mysql %%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(VMYSQL_POOL, vmysql_pool).
-define(PD_VMYSQL_CHANGE, pd_vmysql_change).% 99%通常单条信息操作的表数量不多(少于32), 所以change使用[#vmysql_change{}]
-define(PD_VMYSQL_CACHE, pd_vmysql_cache).
-define(PD_VMYSQL_FLUSH, pd_vmysql_flush).% 每条消息结束后检查, 需要同步的表[table]
-define(VMYSQL_DETS_PATH, "vmysql").% 数据保存到数据失败时保存到dets
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
    table = error({require, table}) :: atom(),% 数据表名和mysql一样
    private_key_pos = error({require, private_key_pos}) :: integer(),% 额外字段, 避免重新构造key, 插入时自动构造
    state_pos = error({require, state_pos}) :: integer(),% 额外字段, 用于缓存中标识数据状态
    select_key = error({require, select_key}) :: [],% 初始化数据库搜索全部数据用的key
    private_key = error({require, private_key}) :: [],% 数据的主键, 至少要有一个字段
    all_fields = error({require, all_fields}) :: [#vmysql_field{}],% 所有列的定义
    record_size = error({require, record_size}) :: integer(),% record的定义, 因为record的字段不一定全都是数据库里面的
    init_fun :: undefined|{M :: atom(), F :: atom()},% fun((Record) -> Record1), 初始化缓存变量
    data = dict:new() :: dict:dict()|{Size :: integer(), list()},% record底层存储类型, 单条数据会转换为tuple, 如果数据量比较大的话使用dict效率更高
    ets_opt = [public, named_table, {write_concurrency, true}] :: list(),% ets参数
    %% 同步策略, 可以同时存在, 互相独立
    sync_time :: undefined|integer(),% 上次同步后N秒再次检查同步
    sync_size :: undefined|integer(),% 变化数据达到N条同步
    
    %% 不用管的, 小优化
    ets :: atom(),
    private_where_sql :: iolist(),
    select_where_sql :: iolist(),
    select_sql :: iolist(),
    replace_sql :: iolist(),
    delete_sql :: iolist()
}).

%% 和#vmysql字段一致
-record(vmysql_change, {
    table :: atom(),
    private_key_pos :: integer(),
    state_pos :: integer(),
    data :: term()
}).

%% hold数据, 可以回滚
-record(vmysql_hold, {
    cache,
    change,
    flush
}).

%% 测试普通主键
-record(vmysql_test_player, {
    vmysql_key,
    vmysql_state,
    player_id,
    str,
    to_str,
    to_bin
}).

%% 测试符合主键
-record(vmysql_test_goods, {
    vmysql_key,
    vmysql_state,
    player_id,
    goods_id,
    str,
    to_str,
    to_bin
}).

