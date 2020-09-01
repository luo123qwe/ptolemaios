%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 数据库用的5.7
%%% 数据缓存和落地
%%% 原则上只有单个进程能够操作数据
%%% 通常一组表由一类进程操作, 并且在进程初始化的时候就加锁
%%% 所以这里的方法不提供锁检测, 需要上层保证加锁
%%% 例如操作某个玩家的数据, 应该为那个玩家加上锁
%%% 而不是仅对某个表加锁
%%% @end
%%%-------------------------------------------------------------------
-module(vmysql).

-include("virture.hrl").
-include("util.hrl").

%% 基本操作
-export([process_init/0, is_load/2, ensure_load/2, load/2, load/3, lookup/2, dirty_lookup/2, insert/1, delete/2, fold_cache/3, make_ets_name/1]).

%% 回滚, 刷到数据库
-export([sync_to_ets/0, sync_to_db/0, hold/0, rollback/1, all_table/0, clean_pd/0, clean_pd/1, clean_ets/0, clean_ets/1]).

%% 算是private
-export([build_table/0, build_table/1, system_init/0, system_init/3, save_defined/0, fix_dets/3, check_dets/0, hotfix/2]).

-type json_def() :: term().
-type field_type() :: int32|int64|uint32|uint64|float|string|to_string|binary|to_binary|json_def().
-export_type([field_type/0]).

%% @doc 自动创建数据表
-spec build_table() -> [{Table :: atom(), Error :: term()|exists|ok}].
build_table() ->
    build_table(virture_config:all(mysql)).

build_table(VirtureList) ->
    lists:map(fun(Virture) ->
        Fields = [[atom_to_list(Field), $ , convert_type(Type), " NOT NULL,"]
            || #vmysql_field{name = Field, type = Type} <- Virture#vmysql.all_fields],
        Index = [[",index(", string:join([atom_to_list(Field) || Field <- IndexField], ","), ")"] || IndexField <- Virture#vmysql.index],
        PrivateKey = ["primary key(", string:join([atom_to_list(Field) || Field <- Virture#vmysql.private_key], ","), $)],
        Sql = ["create table ", atom_to_list(Virture#vmysql.table), "(", Fields, PrivateKey, Index, ")ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"],
%%        io:format("~s~n", [Sql]),
        case mysql_poolboy:query(?VMYSQL_POOL, Sql) of
            {error, {1050, _, _}} ->% 表已经存在
                {Virture#vmysql.table, exists};
            {error, Error} ->
                {Virture#vmysql.table, Error};
            _ ->
                {Virture#vmysql.table, ok}
        end
              end, VirtureList).

convert_type(?VIRTURE_INT32) ->
    "int";
convert_type(?VIRTURE_UINT32) ->
    "int unsigned";
convert_type(?VIRTURE_INT64) ->
    "bigint";
convert_type(?VIRTURE_UINT64) ->
    "bigint unsigned";
convert_type(?VIRTURE_FLOAT) ->
    "float";
convert_type(?VIRTURE_STRING) ->
    "text";
convert_type(?VIRTURE_TO_STRING) ->
    "text";
convert_type(?VIRTURE_BINARY) ->
    "blob";
convert_type(?VIRTURE_TO_BINARY) ->
    "blob";
convert_type(?VIRTURE_JSON_LIST) ->
    "json";
convert_type(?VIRTURE_JSON_OBJ(_)) ->
    "json";
convert_type(?VIRTURE_JSON_OBJ_LIST(_)) ->
    "json".

%% @doc 初始化, system_init
system_init() ->
    system_init(undefined, undefined, undefined).

%% @doc 如果没有数据需要修复Fun传undefined即可, 更多信息看 fix_dets/3
-spec system_init(undefined|function(), undefined|fun((Record :: tuple())-> term()), undefined|function()) -> term().
system_init(Before, Fun, After) ->
    ?LOG_NOTICE("~p", [build_table()]),
    case filelib:is_dir(?VMYSQL_DETS_PATH) of
        true -> ok;
        _ -> file:make_dir(?VMYSQL_DETS_PATH)
    end,
    ?ETS_VMYSQL_LOAD = ets:new(?ETS_VMYSQL_LOAD, [public, named_table]),
    lists:foreach(fun(Virture) ->
        EtsName = make_ets_name(Virture),
        EtsName = ets:new(EtsName, [{keypos, Virture#vmysql.private_key_pos} | lists:keydelete(keypos, 1, Virture#vmysql.ets_opt)])
                  end, virture_config:all(mysql)),
    case fix_dets(Before, Fun, After) of
        ok -> ok;
        {error, Error} -> throw({virture, system_init, Error})
    end.

%% 保存定义
save_defined() ->
    {ok, ?VMYSQL_DETS} = dets:open_file(?VMYSQL_DETS, [{file, ?VMYSQL_DETS_PATH ++ "/" ++ atom_to_list(?VMYSQL_DETS)}, {keypos, #vmysql.table}]),
    lists:foreach(fun(Virture) ->
        dets:insert(?VMYSQL_DETS, Virture)
                  end, virture_config:all(mysql)),
    dets:close(?VMYSQL_DETS).

%% @doc 初始化进程字典
process_init() ->
    case get(?PD_VMYSQL) of
        undefined -> put(?PD_VMYSQL, #{});
        _ -> skip
    end,
    case get(?PD_VMYSQL_NOT_FLUSH) of
        undefined -> put(?PD_VMYSQL_NOT_FLUSH, []);
        _ -> skip
    end.


%% @doc 是否已加载某个表
is_load(Table, SelectKey) ->
    ets:member(?ETS_VMYSQL_LOAD, {Table, SelectKey}).

%% @doc 没加载则加载
ensure_load(Table, SelectKey) ->
    case ets:member(?ETS_VMYSQL_LOAD, {Table, SelectKey}) of
        true -> true;
        _ -> load(Table, SelectKey)
    end.

%% @equiv load(Table, Key, undefined)
load(Table, SelectKey) ->
    load(Table, SelectKey, undefined).

%% @doc 在当前进程初始化一个表
%% 当SelectKey或WhereSql为undefined时, 等效于没有这个where条件
%% WhereSql为额外的where语句, 你需要清楚自己在做什么, 否则请加载到内存进行复杂查询
%% WhereSql=/=[]时总是查询数据库再合本地数据比较
-spec load(atom(), undefiend|list(), undefined|iolist()) -> term().
load(Virture = #vmysql{}, SelectKey, WhereSql) ->
    %% 初始化缓存
    Virture1 = init_virture(Virture),
    %% 初始化数据
    Virture2 = init_data(SelectKey, WhereSql, Virture1),
    VMysql = get(?PD_VMYSQL),
    put(?PD_VMYSQL, VMysql#{Virture#vmysql.table => Virture2});
load(Table, SelectKey, WhereSql) ->
    Virture = virture_config:get(mysql, Table),
    load(Virture, SelectKey, WhereSql).


%% 查询一条数据
-spec lookup(atom(), list()) -> Record :: tuple()|undefined.
lookup(Table, Key) when is_list(Key) ->
    case get(?PD_VMYSQL) of
        #{Table := #vmysql{state_pos = StatePos, data = Data}} ->
            case maps:get(Key, Data, undefined) of
                undefined ->
                    undefined;
                Record ->
                    %% 判断是否已删除
                    case element(StatePos, Record) of
                        ?VIRTURE_STATE_DELETE ->
                            undefined;
                        _ ->
                            Record
                    end
            end;
        _ ->% 没加载
            undefined
    end.


%% @doc 脏读ets, 上层需要保证数据已经加载
-spec dirty_lookup(atom(), list()) -> undefined|tuple().
dirty_lookup(Table, Key) ->
    case ets:lookup(make_ets_name(Table), Key) of
        [Record] ->
            Record;
        _ ->
            undefined
    end.

%% 插入一条数据
-spec insert(tuple()) -> term().
insert(Record) ->
    Table = element(1, Record),
    #{Table := #vmysql{
        state_pos = StatePos, private_key_pos = PrivateKeyPos,
        private_key = PrivateKey, data = Data, change = ChangeMap
    } = Virture} = VMysql = get(?PD_VMYSQL),
    Record1 = setelement(StatePos, Record, ?VIRTURE_STATE_REPLACE),
    %% 新数据自动构造key
    case element(PrivateKeyPos, Record1) of
        undefined ->
            Key = make_private_key(PrivateKey, Record1),
            Record2 = setelement(PrivateKeyPos, Record1, Key);
        Key ->
            Record2 = Record1
    end,
    %% 更新缓存
    Data1 = Data#{Key => Record2},
    %% 更新change
    ChangeMap1 = ChangeMap#{Key => ?VIRTURE_STATE_REPLACE},
    Virture1 = Virture#vmysql{has_change = true, data = Data1, change = ChangeMap1},
    put(?PD_VMYSQL, VMysql#{Table => Virture1}),
    save_not_flush(Table).

%% @doc 删除一条数据
-spec delete(atom(), list()) -> term().
delete(Table, Key) when is_list(Key) ->
    #{Table := #vmysql{
        state_pos = StatePos, private_key_pos = PrivateKeyPos,
        data = Data, change = ChangeMap
    } = Virture} = VMysql = get(?PD_VMYSQL),
    %% 构造record, 不需要全部的record字段, 关键数据在就可以
    Record = erlang:make_tuple(max(StatePos, PrivateKeyPos), undefined, [{1, Table}, {PrivateKeyPos, Key}, {StatePos, ?VIRTURE_STATE_DELETE}]),
    %% 更新缓存
    Data1 = Data#{Key => Record},
    %% 更新change
    ChangeMap1 = ChangeMap#{Key => ?VIRTURE_STATE_DELETE},
    Virture1 = Virture#vmysql{has_change = true, data = Data1, change = ChangeMap1},
    put(?PD_VMYSQL, VMysql#{Table => Virture1}),
    save_not_flush(Table).


%% 记录改变的table
save_not_flush(Table) ->
    NotFlush = get(?PD_VMYSQL_NOT_FLUSH),
    case lists:member(Table, NotFlush) of
        true -> skip;
        _ -> put(?PD_VMYSQL_NOT_FLUSH, [Table | NotFlush])
    end.


%% @doc 遍历当前数据, 支持break
-spec fold_cache(fun((Key :: term(), Record :: term(), Acc :: term())-> Acc1 :: term()), term(), atom()) -> term().
fold_cache(Fun, Acc, Table) ->
    #{Table := #vmysql{state_pos = StatePos, data = Data}} = get(?PD_VMYSQL),
    fold_cache_1(Fun, Acc, StatePos, Data).

%% 从maps复制
fold_cache_1(Fun, Init, StatePos, Map) when is_function(Fun, 3), is_map(Map) ->
    fold_cache_2(Fun, Init, StatePos, maps:iterator(Map)).

fold_cache_2(Fun, Acc, StatePos, Iter) ->
    case maps:next(Iter) of
        {K, V, NextIter} ->
            case element(StatePos, V) of
                ?VIRTURE_STATE_DELETE ->
                    fold_cache_2(Fun, Acc, StatePos, NextIter);
                _ ->
                    case Fun(K, V, Acc) of
                        ?FOLD_BREAK -> Acc;
                        ?FOLD_BREAK_1(Acc1) -> Acc1;
                        Acc1 -> fold_cache_2(Fun, Acc1, StatePos, NextIter)
                    end
            end;
        none ->
            Acc
    end.

%% @doc flush所有改变的数据到ets
-spec sync_to_ets() -> ok.
sync_to_ets() ->
    case get(?PD_VMYSQL_NOT_FLUSH) of
        [] -> ok;
        NotFlush ->
            sync_to_ets(NotFlush, get(?PD_VMYSQL)),
            put(?PD_VMYSQL_NOT_FLUSH, [])
    end.

sync_to_ets([], VMysql) ->
    put(?PD_VMYSQL, VMysql);
sync_to_ets([H | T], VMysql) ->
    Virture = maps:get(H, VMysql),
    Virture1 = do_sync_to_ets(Virture),
    VMysql1 = VMysql#{H => Virture1},
    sync_to_ets(T, VMysql1).

do_sync_to_ets(#vmysql{ets = Ets, data = Data, change = Change} = Virture) ->
    maps:fold(fun(K, S, _Acc) ->
        case S of
            ?VIRTURE_STATE_DELETE ->
                ets:delete(Ets, K);
            ?VIRTURE_STATE_REPLACE ->
                #{K := Record} = Data,
                ets:insert(Ets, Record)
        end
              end, [], Change),
    Virture#vmysql{change = #{}}.

%% @doc 同步到数据库, 同时也会同步到ets
sync_to_db() ->
    VMysql = get(?PD_VMYSQL),
    %% 遍历所有数据
    VMysql1 =
        maps:fold(fun(Table, #vmysql{has_change = true} = Virture, VM) ->
            {HasBadData1, Virture1} = sync_to_db(Virture),
            case HasBadData1 of
                true ->% 小概率事件再遍历一次数据, 不做优化了
                    sync_dets(Virture1);
                _ ->
                    ok
            end,
            VM#{Table => Virture1};
            (_, _, VM) ->% 没有改变
                VM
                  end, VMysql, VMysql),
    put(?PD_VMYSQL, VMysql1).

sync_to_db(#vmysql{
    ets = Ets,
    state_pos = StatePos, data = Data, private_key = PrivateKey, all_fields = AllFields,
    private_where_sql = WhereSql, replace_sql = ReplaceSql, delete_sql = DeleteSql
} = Virture) ->
    %% 更新数据, 拼sql
    {Data1, ReplaceIOList, DeleteIOList, _, LastData, HasBadData} =
        maps:fold(fun(Key, Record, {D, RIO, DIO, N, LD, HBD} = Acc) ->
            try
                case element(StatePos, Record) of
                    ?VIRTURE_STATE_REPLACE ->
                        ets:insert(Ets, Record),
                        Record1 = setelement(StatePos, Record, ?VIRTURE_STATE_NOT_CHANGE),
                        D1 = D#{Key => Record1},
                        [[_, First] | Left] = [[$,, encode(Type, element(Pos, Record))] || #vmysql_field{type = Type, pos = Pos} <- AllFields],
                        RIO1 = [$,, $(, First, Left, $) | RIO],
                        sync_db_check_sql_limit(ReplaceSql, RIO1, DIO, D1, N + 1, LD, HBD);
                    ?VIRTURE_STATE_DELETE ->
                        ets:delete(Ets, Key),
                        D1 = maps:remove(Key, D),
                        WhereSql1 = join_where(WhereSql, PrivateKey, Key),
                        DIO1 = [DeleteSql, WhereSql1, $; | DIO],
                        sync_db_check_sql_limit(ReplaceSql, RIO, DIO1, D1, N + 1, LD, HBD);
                    ?VIRTURE_STATE_NOT_CHANGE ->
                        Acc
                end
            catch
                C:E:S ->
                    ?LOG_ERROR("~w,~w~n~p", [C, E, S]),
                    {LD, [], [], 0, LD, true}
            end
                  end, {Data, [], [], 0, Data, false}, Data),
    Virture1 =
        case do_sync_to_db(ReplaceSql, ReplaceIOList, DeleteIOList) of
            {error, _Reason} ->% 同步失败该段数据整体回滚
                HasBadData1 = true,
                Virture#vmysql{has_change = true, data = LastData, change = #{}};
            _ ->
                HasBadData1 = HasBadData,
                Virture#vmysql{has_change = false, data = Data1, change = #{}}
        end,
    {HasBadData1, Virture1}.


%% 防止sql过长
sync_db_check_sql_limit(ReplaceSql, ReplaceIOList, DeleteIOList, Data, N, LastData, HasBadData) ->
    case N >= ?VMYSQL_SQL_LIMIT andalso do_sync_to_db(ReplaceSql, ReplaceIOList, DeleteIOList) of
        false ->% 还没到限制长度
            {Data, ReplaceIOList, DeleteIOList, N, LastData, HasBadData};
        {error, _Reason} ->% 同步失败该段数据整体回滚
            {LastData, [], [], 0, LastData, true};
        _ ->
            {Data, [], [], 0, Data, HasBadData}
    end.

do_sync_to_db(ReplaceSql, ReplaceIOList, DeleteIOList) ->
    case ReplaceIOList of
        [_ | ReplaceIOList1] -> ok;
        _ -> ReplaceIOList1 = []
    end,
    %% 执行sql
    if
        ReplaceIOList1 =/= [] ->
            mysql_poolboy:query(?VMYSQL_POOL, [ReplaceSql, ReplaceIOList1, $;, DeleteIOList]);
        DeleteIOList =/= [] ->
            mysql_poolboy:query(?VMYSQL_POOL, DeleteIOList);
        true ->
            ok
    end.

%% mysql失败的数据保存到dets
%% 因为失败的是一个个数据段, 所以保存的内容也可能有对有错
sync_dets(#vmysql{table = Table, data = Data, private_key_pos = PKPos}) ->
    case dets:open_file(Table, [{file, ?VMYSQL_DETS_PATH ++ "/" ++ atom_to_list(Table)}, {keypos, PKPos}]) of
        {ok, Table} ->
            maps:fold(fun(_K, V, _) ->
                dets:insert(Table, V)
                      end, [], Data),
            dets:close(Table);
        Error ->
            %% 这里都失败的话没救了
            ?LOG_ERROR("~p", [Error])
    end.

%% @doc 检查dets
check_dets() ->
    {ok, ?VMYSQL_DETS} = dets:open_file(?VMYSQL_DETS, [{file, ?VMYSQL_DETS_PATH ++ "/" ++ atom_to_list(?VMYSQL_DETS)}, {keypos, #vmysql.table}]),
    filelib:fold_files(?VMYSQL_DETS_PATH, ".*", false,
        fun(FileName, _Acc) ->
            Table = list_to_atom(filename:basename(FileName)),
            case Table of
                ?VMYSQL_DETS ->
                    skip;
                _ ->
                    [Virture] = dets:lookup(?VMYSQL_DETS, Table),
                    case dets:open_file(Table, [{file, FileName}, {keypos, Virture#vmysql.private_key_pos}]) of
                        {ok, Table} ->
                            case dets:info(Table, size) > 0 of
                                true ->
                                    throw({vmysql, dets, noempty, Table});
                                _ ->
                                    dets:close(Table),
                                    file:delete(FileName)
                            end;
                        Error ->
                            throw({vmysql, dets, error, Error})
                    end
            end
        end, []).

%% @doc 修复dets数据, 同步到数据库
%% 提供两个钩子, 全部修复开始前, 全部修复成功后
%% Fun函数里面必须包含insert或者delete操作, 否则数据会丢失
%% 如果数据已删除, 则函数传入{delete, key}
%% 简单的修复, A数据在dets, 仅使用正常的数据+坏掉数据A修复
%% 如果涉及复杂修复, 请直接使用dets和sql修复, 因为这里无法处理坏掉数据A+坏掉数据B交叉
-spec fix_dets(undefined|function(), fun((Record :: tuple())-> term()), undefined|function()) -> term().
fix_dets(Before, Fun, After) ->
    Self = self(),
    OldTrapExit = erlang:process_flag(trap_exit, true),
    %% virtue使用进程字典, 使用新进程防止数据混淆
    Spawn =
        spawn_link(fun() ->
            process_init(),
            {ok, ?VMYSQL_DETS} = dets:open_file(?VMYSQL_DETS, [{file, ?VMYSQL_DETS_PATH ++ "/" ++ atom_to_list(?VMYSQL_DETS)}, {keypos, #vmysql.table}]),
            ?DO_IF(is_function(Before), Before()),
            filelib:fold_files(?VMYSQL_DETS_PATH, ".*", false,
                fun(FileName, _Acc) ->
                    Table = list_to_atom(filename:basename(FileName)),
                    case Table of
                        ?VMYSQL_DETS ->
                            skip;
                        _ ->
                            [#vmysql{state_pos = StatePos, private_key_pos = PKPos} = Virture] = dets:lookup(?VMYSQL_DETS, Table),
                            case dets:open_file(Table, [{file, FileName}, {keypos, PKPos}]) of
                                {ok, Table} ->
                                    %% 兼容在线修复
                                    ok = dets:sync(Table),
                                    %% 拿到全部数据
                                    AllRecord = dets:select(Table, [{'_', [], ['$_']}]),
                                    %% 初始化一个空的Virture
                                    Virture1 = init_virture(Virture),
                                    VMysql = get(?PD_VMYSQL),
                                    put(?PD_VMYSQL, VMysql#{Table => Virture1}),
                                    WarpFun = fun(R) ->
                                        case element(StatePos, R) of
                                            ?VIRTURE_STATE_DELETE -> Fun({delete, element(PKPos, R)});
                                            _ -> Fun(R)
                                        end end,
                                    lists:foreach(WarpFun, AllRecord),
                                    %% 同步到数据库
                                    #{Table := Virture2} = get(?PD_VMYSQL),
                                    case sync_to_db(Virture2) of
                                        {true, _} ->% 还是有错误, 中断
                                            throw({vmysql, dets, db, fail});
                                        {_, Virture3} ->
                                            VMysql1 = get(?PD_VMYSQL),
                                            put(?PD_VMYSQL, VMysql1#{Table => Virture3}),
                                            %% 删除dets
                                            dets:close(Table),
                                            file:delete(FileName)
                                    end;
                                Error ->
                                    throw({vmysql, dets, error, Error})
                            end
                    end
                end, []),
            dets:close(?VMYSQL_DETS),
            ?DO_IF(is_function(After), After()),
            Self ! fix
                   end),
    Result =
        receive
            {'EXIT', Spawn, Reason} ->
                {error, Reason};
            fix ->
                %% 更新定义
                save_defined(),
                ok
        end,
    erlang:process_flag(trap_exit, OldTrapExit),
    Result.

%% @doc 获取当前状态, 用于回滚
-spec hold() -> {list(), map()}.
hold() ->
    {get(?PD_VMYSQL_NOT_FLUSH), get(?PD_VMYSQL)}.

%% @doc hold()的状态回滚
-spec rollback({list(), map()}) -> term().
rollback({NotFlush, VMysql}) ->
    put(?PD_VMYSQL_NOT_FLUSH, NotFlush),
    put(?PD_VMYSQL, VMysql).

%% @doc 清理缓存数据
clean_pd() ->
    put(?PD_VMYSQL, #{}),
    put(?PD_VMYSQL_NOT_FLUSH, []).

%% @doc 清理缓存
-spec clean_pd(list()) -> term().
clean_pd(TableList) ->
    clean(TableList, get(?PD_VMYSQL), get(?PD_VMYSQL_NOT_FLUSH)).

clean([], VMysql, NotFlush) ->
    put(?PD_VMYSQL, VMysql),
    put(?PD_VMYSQL_NOT_FLUSH, NotFlush),
    ok;
clean([Table | T], VMysql, NotFlush) ->
    clean(T, maps:remove(Table, VMysql), lists:delete(Table, NotFlush)).

clean_ets() ->
    TableList = lists:usort(ets:select(?ETS_VMYSQL_LOAD, [{{{'$1', '_'}}, [], ['$1']}])),
    lists:foreach(fun(Table) ->
        ets:delete_all_objects(vmysql:make_ets_name(Table))
                  end, TableList),
    ets:delete_all_objects(?ETS_LOCAL_LOCK).

clean_ets([]) ->
    [];
clean_ets([Table | T]) ->
    ets:delete_all_objects(vmysql:make_ets_name(Table)),
    ets:select_delete(?ETS_VMYSQL_LOAD, [{{{Table, '_'}}, [], [true]}]),
    clean_ets(T).

%% @doc 获取全部table
-spec all_table() -> list().
all_table() ->
    case get(?PD_VMYSQL) of
        VMysql when is_map(VMysql) -> maps:keys(VMysql);
        _ -> []
    end.

%% @doc 热修复某个表
%% 如果定义变了(is_diff_def/2) ,Fun函数里面必须包含insert或者delete操作, 否则数据会丢失
%% 因为ets热修复不会进行重置
%% 所以ets参数不会覆盖, private_key_pos不能改变
%% 简单的修复, A数据在缓存, 仅使用正常的数据+坏掉数据A修复
%% 如果涉及复杂修复, 请直接使用fold_cache和ets和sql修复, 因为这里无法处理坏掉数据A+坏掉数据B交叉
hotfix(Table, Fun) ->
    Hold = hold(),
    #{Table := Virture} = VMysql = get(?PD_VMYSQL),
    case catch do_hotfix(Fun, Virture, VMysql) of
        ok ->
            ok;
        Error ->
            ?LOG_ERROR("hotfix~n~p", [Error]),
            rollback(Hold),
            error
    end.

do_hotfix(Fun, #vmysql{table = Table, state_pos = StatePos, private_key_pos = PKPos, data = Data} = OldVirture, VMysql) ->
    {ok, ?VMYSQL_DETS} = dets:open_file(?VMYSQL_DETS, [{file, ?VMYSQL_DETS_PATH ++ "/" ++ atom_to_list(?VMYSQL_DETS)}, {keypos, #vmysql.table}]),
    case virture_config:get(mysql, Table) of
        #vmysql{} = NewConfig ->
            case PKPos == NewConfig#vmysql.private_key_pos of
                true -> skip;
                _ ->
                    %% 主键位置不能改变因为ets无法中途变更
                    throw({vmysql, Table, private_key_pos, cant, change})
            end,
            NewVirture =
                case is_same_def(OldVirture, NewConfig) of
                    true ->% 数据定义没改变, 杂项更新
                        OldVirture#vmysql{
                            init_fun = NewConfig#vmysql.init_fun
                        };
                    _ ->
                        %% 初始化新的
                        NewVirture_1 = init_virture(NewConfig),
                        %% 老数据赋值回去
                        NewVirture_1#vmysql{
                            data = Data,
                            change = OldVirture#vmysql.change,
                            has_change = OldVirture#vmysql.has_change
                        }
                end,
            VMsql1 = VMysql#{Table => NewVirture},
            put(?PD_VMYSQL, VMsql1),
            %% 执行Fun
            WarpFun = fun(K, R, _Acc) ->
                case element(StatePos, R) of
                    ?VIRTURE_STATE_DELETE -> Fun({delete, K});
                    _ -> Fun(R)
                end end,
            maps:fold(WarpFun, [], Data),
            %% 全部成功, 更新定义
            dets:insert(?VMYSQL_DETS, NewConfig),
            dets:close(?VMYSQL_DETS);
        _ ->% 表被删除了
            VMsql1 = maps:remove(Table, VMysql),
            put(?PD_VMYSQL, VMsql1),
            put(?PD_VMYSQL_NOT_FLUSH, lists:delete(Table, get(?PD_VMYSQL_NOT_FLUSH))),
            dets:delete(?VMYSQL_DETS, Table),
            dets:close(?VMYSQL_DETS)
    end,
    ok.

%% 定义是否改变
is_same_def(Old, New) ->
    Old#vmysql.state_pos == New#vmysql.state_pos
        andalso Old#vmysql.select_key == New#vmysql.select_key
        andalso Old#vmysql.private_key == New#vmysql.private_key
        andalso Old#vmysql.all_fields == New#vmysql.all_fields
        andalso Old#vmysql.record_size == New#vmysql.record_size.

%% 初始化
init_virture(#vmysql{all_fields = AllField, private_key = PrivateKey, select_key = Selectkey} = Virture) ->
    %% 初始化优化sql
    Virture#vmysql{
        ets = make_ets_name(Virture),
        private_key = [lists:keyfind(Field, #vmysql_field.name, AllField) || Field <- PrivateKey],
        select_key = [lists:keyfind(Field, #vmysql_field.name, AllField) || Field <- Selectkey],
        private_where_sql = make_private_where_sql(Virture),
        select_where_sql = make_select_where_sql(Virture),
        select_sql = make_select_sql(Virture),
        replace_sql = make_replace_sql(Virture),
        delete_sql = make_delete_sql(Virture)
    }.

%% ets名字
make_ets_name(#vmysql{table = Table}) ->
    make_ets_name(Table);
make_ets_name(Table) ->
    list_to_atom("ets_" ++ atom_to_list(Table)).

make_private_where_sql(Virture) ->
    case Virture#vmysql.private_key of
        [H] ->
            [" WHERE " ++ atom_to_list(H) ++ "="];
        [H | T] ->
            HSql = " WHERE " ++ atom_to_list(H) ++ "=",
            TSql = [" AND " ++ atom_to_list(Name) ++ "=" || Name <- T],
            [HSql | TSql]
    end.

make_select_where_sql(Virture) ->
    case Virture#vmysql.select_key of
        [] ->
            [];
        [H] ->
            [" WHERE " ++ atom_to_list(H) ++ "="];
        [H | T] ->
            HSql = " WHERE " ++ atom_to_list(H) ++ "=",
            TSql = [" AND " ++ atom_to_list(Name) ++ "=" || Name <- T],
            [HSql | TSql]
    end.

make_select_sql(Virture) ->
    AllField = string:join([atom_to_list(VK#vmysql_field.name) || VK <- Virture#vmysql.all_fields], ","),
    "SELECT " ++ AllField ++ " FROM " ++ atom_to_list(Virture#vmysql.table).

make_replace_sql(Virture) ->
    AllField = string:join([atom_to_list(VK#vmysql_field.name) || VK <- Virture#vmysql.all_fields], ","),
    ["REPLACE INTO " ++ atom_to_list(Virture#vmysql.table) ++ " (" ++ AllField ++ ")VALUES"].

make_delete_sql(Virture) ->
    "DELETE FROM " ++ atom_to_list(Virture#vmysql.table).

%% 拼where的sql
join_where([], [], []) ->
    [];
join_where([SqlH | SqlT], [#vmysql_field{type = Type} | FieldT], [Value | ValueT]) ->
    [SqlH, encode(Type, Value) | join_where(SqlT, FieldT, ValueT)].

%% 初始化数据
init_data(SelectValue, ExtWhereSql, Virture) ->
    #vmysql{
        table = Table, ets = EtsName,
        private_key_pos = PrivateKeyPos, state_pos = StatePos,
        private_key = PrivateKey, select_key = SelectKey,
        select_where_sql = WhereSql, select_sql = SelectSql,
        record_size = RecordSize,
        data = VData, init_fun = InitFun
    } = Virture,
    case ExtWhereSql =/= undefined orelse SelectValue == undefined orelse is_load(Table, SelectValue) == false of
        true ->
            %% 从数据库拿
            WhereSql1 =
                case SelectValue of
                    undefined ->
                        case ExtWhereSql of
                            undefined -> [];
                            _ -> [" WHERE ", ExtWhereSql]
                        end;
                    _ ->
                        case ExtWhereSql of
                            undefined -> join_where(WhereSql, SelectKey, SelectValue);
                            _ -> [join_where(WhereSql, SelectKey, SelectValue), " AND ", ExtWhereSql]
                        end
                end,
            {ok, _ColumnNames, Rows} = mysql_poolboy:query(?VMYSQL_POOL, [SelectSql, WhereSql1]),
            %% 构造record数据
            Data =
                lists:foldl(fun(Row, Acc) ->
                    Record = init_record(InitFun, Row, Virture),
                    %% 构造key
                    K = make_private_key(PrivateKey, Record),
                    Record1 = setelement(PrivateKeyPos, Record, K),
                    %% 设置状态
                    Record2 = erlang:setelement(StatePos, Record1, ?VIRTURE_STATE_NOT_CHANGE),
                    Acc#{K => Record2}
                            end, VData, Rows),
            %% 全部初始化成功再放到ets
            Data1 =
                maps:fold(fun(K, Value, Acc) ->
                    %% 读进程可以很多个, 但是写进程只有一个
                    %% 保证初始化不会覆盖即可
                    case ets:insert_new(EtsName, Value) of
                        true -> Acc;
                        false ->
                            %% 缓存中已经有数据
                            case ets:lookup(EtsName, K) of
                                [Value1] -> Acc#{K => Value1};
                                _ -> maps:remove(K, Acc)
                            end
                    end
                          end, Data, Data),
            ets:insert(?ETS_VMYSQL_LOAD, {{Table, SelectValue}}),
            Virture#vmysql{data = Data1};
        _ ->
            Spec = erlang:make_tuple(RecordSize, '_'),
            KeySpec = make_ets_key_spec(PrivateKey, SelectKey, SelectValue),
            Spec1 = setelement(PrivateKeyPos, Spec, KeySpec),
            EtsData = ets:select(EtsName, [{Spec1, [], ['$_']}]),
            %% 构造本地缓存
            Data =
                lists:foldl(fun(Record, Acc) ->
                    %% flush时不会重新设置state
                    Acc#{element(PrivateKeyPos, Record) => setelement(StatePos, Record, ?VIRTURE_STATE_NOT_CHANGE)}
                            end, VData, EtsData),
            Virture#vmysql{data = Data}
    end.


make_private_key(PrivateKey, Record) ->
    [element(Pos, Record) || #vmysql_field{pos = Pos} <- PrivateKey].


make_ets_key_spec([], _SelectKey, _Value) ->
    [];
make_ets_key_spec([#vmysql_field{pos = Pos} | PKT], SelectKey, ValueList) ->
    case make_ets_key_spec_find_value(Pos, SelectKey, ValueList) of
        false ->
            ['_' | make_ets_key_spec(PKT, SelectKey, ValueList)];
        Value ->
            [Value | make_ets_key_spec(PKT, SelectKey, ValueList)]
    end.

make_ets_key_spec_find_value(_Pos, [], []) ->
    false;
make_ets_key_spec_find_value(Pos, [#vmysql_field{pos = Pos} | _FT], [Value | _VT]) ->
    Value;
make_ets_key_spec_find_value(Pos, [_ | FT], [_ | VT]) ->
    make_ets_key_spec_find_value(Pos, FT, VT).

encode(to_string, Value) ->
    io_lib:format("'~w'", [Value]);
%% 用的数据库是5.7, 加上_binary才不会报Warning 1300: Invalid utf8mb4 character string
encode(to_binary, Value) ->
    ["_binary ", mysql_encode:encode(erlang:term_to_binary(Value))];
encode(binary, Value) ->
    ["_binary ", mysql_encode:encode(Value)];
encode(?VIRTURE_JSON_LIST, List) ->
    encode_json_list(List);
encode(?VIRTURE_JSON_OBJ(NameList), Value) ->
    encode_json_obj(NameList, Value);
encode(?VIRTURE_JSON_OBJ_LIST(NameList), Value) ->
    encode_json_obj_list(NameList, Value);
encode(_, Value) ->
    mysql_encode:encode(Value).

encode_json_list(List) ->
    ["JSON_ARRAY(", do_encode_json_list(List), $)].

do_encode_json_list([]) ->
    [];
do_encode_json_list([H]) ->
    mysql_encode:encode(H);
do_encode_json_list([H | T]) ->
    [mysql_encode:encode(H), $, | do_encode_json_list(T)].

encode_json_obj(NameList, Value) ->
    ["JSON_OBJECT(", do_encode_json_obj(NameList, tuple_to_list(Value)), $)].

do_encode_json_obj([], []) ->
    [];
do_encode_json_obj([N], [V]) ->
    [do_encode_json_single_obj(N, V)];
do_encode_json_obj([N | NT], [V | VT]) ->
    [do_encode_json_single_obj(N, V), $, | do_encode_json_obj(NT, VT)].

encode_json_obj_list(NameList, List) ->
    ["JSON_ARRAY(", do_encode_json_obj_list(NameList, List), $)].

do_encode_json_obj_list(_NameList, []) ->
    [];
do_encode_json_obj_list(NameList, [V]) ->
    encode_json_obj(NameList, V);
do_encode_json_obj_list(NameList, [V | VT]) ->
    [encode_json_obj(NameList, V), $, | do_encode_json_obj_list(NameList, VT)].

do_encode_json_single_obj(?VIRTURE_JSON_LIST(Name), V) ->
    [$', Name, $', $,, encode_json_list(V)];
do_encode_json_single_obj(?VIRTURE_JSON_OBJ(Name, NameList), V) ->
    [$', Name, $', $,, encode_json_obj(NameList, V)];
do_encode_json_single_obj(?VIRTURE_JSON_OBJ_LIST(Name, NameList), V) ->
    [$', Name, $', $,, encode_json_obj_list(NameList, V)];
do_encode_json_single_obj(Name, V) ->
    [$', Name, $', $,, mysql_encode:encode(V)].


decode(to_string, Value) ->
    util:eval(Value);
decode(to_binary, Value) ->
    erlang:binary_to_term(Value);
decode(?VIRTURE_JSON_LIST, Value) ->
    jsx:decode(Value);
decode(?VIRTURE_JSON_OBJ(NameList), Value) ->
    decode_json_obj(NameList, jsx:decode(Value));
decode(?VIRTURE_JSON_OBJ_LIST(NameList), Value) ->
    decode_json_obj_list(NameList, jsx:decode(Value));
decode(_, Value) ->
    Value.

decode_json_obj(NameList, Map) ->
    list_to_tuple([decode_json_single_obj(Name, Map) || Name <- NameList]).

decode_json_obj_list(NameList, List) ->
    [decode_json_obj(NameList, Obj) || Obj <- List].

decode_json_single_obj(?VIRTURE_JSON_LIST(Name), Map) ->
    jsx:decode(maps:get(Name, Map));
decode_json_single_obj(?VIRTURE_JSON_OBJ(Name, NameList), Map) ->
    decode_json_obj(NameList, maps:get(Name, Map));
decode_json_single_obj(?VIRTURE_JSON_OBJ_LIST(Name, NameList), Map) ->
    decode_json_obj_list(NameList, maps:get(Name, Map));
decode_json_single_obj(Name, Map) ->
    maps:get(Name, Map).

%% 初始化record
init_record(Init, Row, Virture) ->
    BaseRecord = erlang:make_tuple(Virture#vmysql.record_size, undefined, [{1, Virture#vmysql.table}]),
    init_record(Init, Row, Virture#vmysql.all_fields, BaseRecord).

init_record(InitFun, [], [], Record) ->
    case InitFun of
        {M, F} -> M:F(Record);
        _ -> Record
    end;
init_record(InitFun, [Value | ValueT], [#vmysql_field{pos = Pos, type = Type} | FieldT], Record) ->
    init_record(InitFun, ValueT, FieldT, setelement(Pos, Record, decode(Type, Value))).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

base_test_() ->
    {setup,
        fun() ->
            application:start(poolboy),
            application:start(mysql),
            application:start(mysql_poolboy),
            PoolOptions = [{size, 50}, {max_overflow, 100}],
            MySqlOptions = [{user, "root"}, {password, "123456"}, {database, "test"},
                {keep_alive, true},
                {prepare, []}],
            mysql_poolboy:add_pool(?VMYSQL_POOL, PoolOptions, MySqlOptions),
            %% 删除test数据表
            mysql_poolboy:query(?VMYSQL_POOL, "drop table if exists vmysql_test_player;
    drop table if exists vmysql_test_goods;"),
            %% build
            vmysql:system_init()
        end,
        fun(_) ->
            application:stop(mysql_poolboy),
            application:stop(poolboy),
            application:stop(mysql)
        end,
        fun(_) ->
            %% 懒得写那么多宏了
            %% 初始化
            vmysql:process_init(),
            vmysql:load(vmysql_test_player, [1]),
            vmysql:load(vmysql_test_goods, [1]),
            %% 插入数据
            undefined = vmysql:lookup(vmysql_test_player, [1]),
            undefined = vmysql:lookup(vmysql_test_goods, [1, 1]),
            vmysql:insert(#vmysql_test_player{player_id = 1, str = <<"1">>, to_str = <<"1">>, to_bin = <<"1">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vmysql:insert(#vmysql_test_player{player_id = 2, str = <<"2">>, to_str = <<"2">>, to_bin = <<"2">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vmysql:insert(#vmysql_test_goods{player_id = 1, goods_id = 1, str = <<"1">>, to_str = <<"1">>, to_bin = <<"1">>}),
            vmysql:insert(#vmysql_test_goods{player_id = 1, goods_id = 2, str = <<"2">>, to_str = <<"2">>, to_bin = <<"2">>}),
            vmysql:insert(#vmysql_test_goods{player_id = 2, goods_id = 1, str = <<"1">>, to_str = <<"1">>, to_bin = <<"1">>}),
            vmysql:insert(#vmysql_test_goods{player_id = 2, goods_id = 2, str = <<"2">>, to_str = <<"2">>, to_bin = <<"2">>}),
            0 = ets:info(vmysql:make_ets_name(vmysql_test_player), size),
            0 = ets:info(vmysql:make_ets_name(vmysql_test_goods), size),
            vmysql:sync_to_db(),
            2 = ets:info(vmysql:make_ets_name(vmysql_test_player), size),
            4 = ets:info(vmysql:make_ets_name(vmysql_test_goods), size),
%%    ?debugFmt("~p~n", [get()]),
            %% 插入+更新+删除
            vmysql:insert(#vmysql_test_player{player_id = 1, str = <<"11">>, to_str = <<"11">>, to_bin = <<"11">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vmysql:delete(vmysql_test_player, [2]),
            vmysql:insert(#vmysql_test_goods{player_id = 1, goods_id = 1, str = <<"11">>, to_str = <<"11">>, to_bin = <<"11">>}),
            vmysql:delete(vmysql_test_goods, [1, 2]),
            vmysql:insert(#vmysql_test_player{player_id = 3, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vmysql:insert(#vmysql_test_goods{player_id = 3, goods_id = 1, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>}),
            vmysql:insert(#vmysql_test_goods{player_id = 3, goods_id = 2, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>}),
            #vmysql_test_player{str = <<"11">>} = vmysql:lookup(vmysql_test_player, [1]),
            undefined = vmysql:lookup(vmysql_test_player, [2]),
            #vmysql_test_player{str = <<"3">>} = vmysql:lookup(vmysql_test_player, [3]),
            #vmysql_test_goods{str = <<"11">>} = vmysql:lookup(vmysql_test_goods, [1, 1]),
            undefined = vmysql:lookup(vmysql_test_goods, [1, 2]),
            #vmysql_test_goods{str = <<"3">>} = vmysql:lookup(vmysql_test_goods, [3, 1]),
%%    ?debugFmt("~p~n", [get()]),
            vmysql:sync_to_db(),
            2 = ets:info(vmysql:make_ets_name(vmysql_test_player), size),
            5 = ets:info(vmysql:make_ets_name(vmysql_test_goods), size),
            #vmysql_test_player{str = <<"11">>} = vmysql:lookup(vmysql_test_player, [1]),
            undefined = vmysql:lookup(vmysql_test_player, [2]),
            #vmysql_test_player{str = <<"3">>} = vmysql:lookup(vmysql_test_player, [3]),
            #vmysql_test_goods{str = <<"11">>} = vmysql:lookup(vmysql_test_goods, [1, 1]),
            undefined = vmysql:lookup(vmysql_test_goods, [1, 2]),
            #vmysql_test_goods{str = <<"3">>} = vmysql:lookup(vmysql_test_goods, [3, 1]),
            %% hold
            Hold = vmysql:hold(),
            vmysql:insert(#vmysql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vmysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 1, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            vmysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 2, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            #vmysql_test_player{} = vmysql:lookup(vmysql_test_player, [4]),
            #vmysql_test_goods{} = vmysql:lookup(vmysql_test_goods, [4, 1]),
            #vmysql_test_goods{} = vmysql:lookup(vmysql_test_goods, [4, 2]),
            vmysql:rollback(Hold),
            2 = ets:info(vmysql:make_ets_name(vmysql_test_player), size),
            5 = ets:info(vmysql:make_ets_name(vmysql_test_goods), size),
            undefined = vmysql:lookup(vmysql_test_player, [4]),
            undefined = vmysql:lookup(vmysql_test_goods, [4, 1]),
            undefined = vmysql:lookup(vmysql_test_goods, [4, 2]),
            %% all_table
            [] = vmysql:all_table()--[vmysql_test_player, vmysql_test_goods],
            vmysql:clean_pd(vmysql:all_table()),
            [] = vmysql:all_table(),
            %% sync
            vmysql:load(vmysql_test_player, [1]),
            vmysql:load(vmysql_test_goods, [1]),
            vmysql:insert(#vmysql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vmysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 1, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            vmysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 2, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            2 = ets:info(vmysql:make_ets_name(vmysql_test_player), size),
            5 = ets:info(vmysql:make_ets_name(vmysql_test_goods), size),
            sync_to_db(maps:get(vmysql_test_goods, get(?PD_VMYSQL))),
            2 = ets:info(vmysql:make_ets_name(vmysql_test_player), size),
            7 = ets:info(vmysql:make_ets_name(vmysql_test_goods), size),
            sync_to_db(maps:get(vmysql_test_goods, get(?PD_VMYSQL))),
            2 = ets:info(vmysql:make_ets_name(vmysql_test_player), size),
            7 = ets:info(vmysql:make_ets_name(vmysql_test_goods), size),
            %% 初始化
            vmysql:clean_ets([vmysql_test_player]),
            vmysql:clean_pd(),
            %% dets
            vmysql:load(vmysql_test_player, [4]),
            %% 没有json, sync会失败, 会有一个正常报错
            vmysql:insert(#vmysql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            sync_to_db(),
            vmysql:clean_pd(),
            vmysql:fix_dets(undefined, fun(R) ->
                R1 = R#vmysql_test_player{to_json = [{1, {2, 3}}, {11, {22, 33}}]},
                vmysql:insert(R1)
                                              end, undefined),
            [#vmysql_test_player{}] = ets:lookup(vmysql:make_ets_name(vmysql_test_player), [4]),
            vmysql:clean_pd([vmysql_test_player]),
            vmysql:clean_ets([vmysql_test_player]),
            vmysql:load(vmysql_test_player, [4]),
            #vmysql_test_player{} = vmysql:lookup(vmysql_test_player, [4]),
            %% dirty lookup
            vmysql:clean_ets([vmysql_test_goods]),
            vmysql:clean_pd(),
            undefined = vmysql:dirty_lookup(vmysql_test_goods, [4, 1]),
            vmysql:load(vmysql_test_goods, [4]),
            clean_pd(),
            #vmysql_test_goods{} = vmysql:dirty_lookup(vmysql_test_goods, [4, 1]),
            %% fold_cache
            vmysql:load(vmysql_test_goods, [4]),
            2 = fold_cache(fun(_K, _V, Acc) ->
                Acc + 1
                           end, 0, vmysql_test_goods),
            vmysql:insert(#vmysql_test_goods{player_id = 4, goods_id = 3, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            3 = fold_cache(fun(_K, _V, Acc) ->
                Acc + 1
                           end, 0, vmysql_test_goods),
            %% 热更新定义, shell 执行
            %% vmysql_test_goods的to_str改成string类型
            %% os:cmd("rebar3 compile"), rr("include/*"), l(vmysql), l(virture_config).
            %% vmysql:hotfix(vmysql_test_goods, fun(R) -> vmysql:insert(R#vmysql_test_goods{to_str = <<"hotfix">>}) end).
            %% #vmysql_test_goods{to_str = <<"hotfix">>} = vmysql:lookup(vmysql_test_goods, [4, 1]).
            %% vmysql:sync().
            %% 看数据库
            %% vmysql:clean(), vmysql:clean_ets().
            %% vmysql:load(vmysql_test_goods, [4]), vmysql:lookup(vmysql_test_goods, [4, 1]).
            []
        end}.

-endif.