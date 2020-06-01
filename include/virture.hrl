-record(virture_state, {
    index,% 服务器索引
    virture,% #virture
    keys = []% 哪些key需要落地, 超过100可能要用dict代替
}).

-record(virture, {
    %% 必须
    server_num :: integer(),% 同步进程数量, 名字为virture_tab_i
    table :: atom(),% 数据库表名和ets名字一样
    ets_key_pos :: integer(),% 数据库key位置
    %% Type = string | binary | Other :: term()
    %% string = 把term转换成string, io_lib:format("~w", [Term])
    %% binary = 把term转换成binary, erlang:term_to_binary(Term)
    key :: [{FieldName :: atom(), RecordIndex :: integer(), Type :: atom()}],% 唯一键
    fields :: [{FieldName :: atom(), RecordIndex :: integer(), Type :: atom()}],% 同步字段, 不包含上面的
    %% 可选
    %% 根据key获得record, ([{FieldName, RecordIndex}]]) => [Record]
    %% 默认, select field1, field2 from tab where key1 = 1 and key2 = 2
    select_fun :: {Module :: atom(), Function :: atom()},
    %% 初始化record的其他参数, ([Fields]) => Record1
    %% 默认, {tab, field1, field2,,,,}, 最大长度等于key++fields下标最大值
    record_fun :: {Module :: atom(), Function :: atom()},
    %% 插入数据库的sql, (Record) => Sql
    %% 默认, insert into tab (key1,key2,field1,field2)VALUES(1,2,1,2)
    insert_fun :: {Module :: atom(), Function :: atom()},
    %% 更新数据库的sql, (Record) => Sql
    %% 默认, update tab set field1 = 1, field2 = 2 where key1 = 1 and key2 = 2
    update_fun :: {Module :: atom(), Function :: atom()},
    %% 从数据库删除的sql, ([{FieldName, RecordIndex}]) => Sql
    %% 默认, delete from tab where key1 = 1 and key2 = 2
    delete_fun :: {Module :: atom(), Function :: atom()},
    
    min_size,% 哈希数组长度({[],[],,,,}), 推荐预估数量/16
    sync_interval = 1000,% mysql同步检查间隔(ms)
    sync_num = 50% 同步sql数量达到N条同步数据库
}).









