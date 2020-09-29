%% 数据状态
-define(VT_STATE_DELETE, 0).
-define(VT_STATE_REPLACE, 1).
-define(VT_STATE_NOT_CHANGE, 2).

%% 带着size的列表结构
-define(VT_LIST, {0, []}).
-define(VT_LIST(Size, List), {Size, List}).

%% 数据类型
-define(VT_INT32, int32).
-define(VT_UINT32, uint32).
-define(VT_INT64, int64).
-define(VT_UINT64, uint64).
-define(VT_FLOAT, float).
-define(VT_STRING, string).
-define(VT_TO_STRING, to_string).
-define(VT_BINARY, binary).
-define(VT_TO_BINARY, to_binary).
%% JSON结构定义
%% 支持
%% 元组 => obj, {1,2} => {"a":1,"b":2}
%% 列表 => 列表, [1,a] => [1, "a"]
%% 元组列表 => 对象列表, [{1,2}] => [{"a":1,"b":2}]
%% 不支持列表里面混合一般元素和obj
-define(VT_JSON_LIST, json_list).% 列表, ["a", 1, VT_JSON_嵌套用]
-define(VT_JSON_LIST(Name), {json_list, Name}).% 嵌套用
-define(VT_JSON_OBJ(NameList), {json_obj, NameList}).% 元组, {"a":1, "b":1, VT_JSON_嵌套用}
-define(VT_JSON_OBJ(Name, NameList), {json_obj, Name, NameList}).% 嵌套用
-define(VT_JSON_OBJ_LIST(NameList), {json_obj_list, NameList}).% 列表元组, [{"a":1, "b":1}]
-define(VT_JSON_OBJ_LIST(Name, NameList), {json_obj_list, Name, NameList}).% 嵌套用


%% 一份代码相当于对应一个数据库和一个连接池
%% 所以如果需要使用多个数据库和连接池的话, 复制一份代码并且替换宏即可
%%%%%%%%%%%%%%%%%%%%%%%%%%%% virture mysql %%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(VT_SQL_POOL, vt_sql_pool).
-define(PD_VT_SQL, pd_vt_sql).%% #{table => #vt_sql{}}
-define(PD_VT_SQL_NOT_FLUSH, pd_vt_sql_not_flush).%% [table], 记录改变数据且未同步到ets的table, 避免每次遍历全部表
-define(ETS_VT_SQL_LOAD, ets_vt_sql_load).% 已经加载的数据, {{table, key}, 1}
% 数据保存到数据失败时保存到dets
-define(DETS_VT_SQL, vt_sql).% 配置保存表名
-define(VT_SQL_SQL_LIMIT, 1000).% 单次操作拼sql的条数
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% field
-record(vt_sql_field, {
    name :: atom(),% 对应数据库字段名
    type :: virture:field_type(),% 数据类型, 自动建表用
    pos :: integer()% 对应record的下标
}).

%% table
-record(vt_sql, {
    %% 配置项
    table = error({require, table}) :: atom(),% 数据表名和mysql一样
    use_ets = true :: boolean(),% 是否使用ets作为缓存
    private_key_pos = error({require, private_key_pos}) :: integer(),% 额外字段, 避免重新构造key, 插入时自动构造
    state_pos = error({require, state_pos}) :: integer(),% 额外字段, 用于缓存中标识数据状态
    select_key = error({require, select_key}) :: [],% 初始化数据库搜索全部数据用的key
    private_key = error({require, private_key}) :: [],% 数据的主键, 至少要有一个字段
    all_fields = error({require, all_fields}) :: [#vt_sql_field{}],% 所有列的定义
    index = [] :: [[Key :: atom()]],%  自动创建数据库的时候加索引
    record_size = error({require, record_size}) :: integer(),% record的定义, 因为record的字段不一定全都是数据库里面的
    init_fun :: undefined|{M :: atom(), F :: atom()},% fun((Record) -> Record1), 初始化缓存变量
    ets_opt = [public, named_table, {write_concurrency, true}] :: list(),% ets参数
    sync_size :: undefined|integer(),% 变化数据达到N条同步
    
    %% 不用管的
    ets :: atom(),
    data = #{},% #{key => record}
    change = #{},% #{key => delete|change}
    private_where_sql :: iolist(),
    select_where_sql :: iolist(),
    select_sql :: iolist(),
    replace_sql :: iolist(),
    delete_sql :: iolist(),
    has_change :: boolean()%% 是否有改变, 用于sync
}).

%% 测试普通主键
-record(vt_sql_test_player, {
    vt_sql_key,
    vt_sql_state,
    player_id,
    str,
    to_str,
    to_bin,
    to_json
}).

%% 测试复合主键
-record(vt_sql_test_goods, {
    vt_sql_key,
    vt_sql_state,
    player_id,
    goods_id,
    str,
    to_str,
    to_bin
}).

%% 测试不使用ets缓存
-record(vt_sql_test_equip, {
    vt_sql_key,
    vt_sql_state,
    player_id,
    equip_id
}).

