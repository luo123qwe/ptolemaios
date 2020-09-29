%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 数据存储, 使用mysql作为数据库
%%%
%%% mysql版本的5.7, 支持json
%%%
%%% 设计上同一时间只有一个进程能够写某条数据
%%%
%%% 例如操作某个玩家的数据, 只能由该玩家进程进行写, 其他进程脏读
%%%
%%% 但是修复数据的时候, 是由修复数据的进程进行写, 因为管理数据的进程可能没开启
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(vt_sql).

-include("vt.hrl").
-include("util.hrl").

%% 基本操作
-export([process_init/0, is_load_ets/2, ensure_load_ets/2, load/2, load/3, lookup/2, lookup_ets/2, insert/1, delete/2, fold_cache/3, make_ets_name/1]).

%% 回滚, 刷到数据库
-export([sync_to_ets/0, sync_to_db/0, hold/0, rollback/1, all_table/0, clean_pd/0, clean_pd/1, clean_ets/0, clean_ets/1]).

%% 算是private
-export([build_table/0, build_table/1, system_init/0, system_init/3, save_defined/0, fix_dets/3, check_dets/0, hotfix/2]).

%% 数据库结构
-export([query/1]).

-type json_def() :: term().
-type field_type() :: int32|int64|uint32|uint64|float|string|to_string|binary|to_binary|json_def().
-export_type([field_type/0]).

%% @private 自动创建数据表
-spec build_table() -> [{Table :: atom(), Error :: term()|exists|ok}].
build_table() ->
    build_table(vt:all(mysql)).

%% @private 自动创建数据表
-spec build_table([#vt_sql{}]) -> [{Table :: atom(), Error :: term()|exists|ok}].
build_table(VirtureList) ->
    lists:map(fun(Virture) ->
        Fields = [[atom_to_list(Field), $ , convert_type(Type), " NOT NULL,"]
            || #vt_sql_field{name = Field, type = Type} <- Virture#vt_sql.all_fields],
        Index = [[",index(", string:join([atom_to_list(Field) || Field <- IndexField], ","), ")"] || IndexField <- Virture#vt_sql.index],
        PrivateKey = ["primary key(", string:join([atom_to_list(Field) || Field <- Virture#vt_sql.private_key], ","), $)],
        Sql = ["create table ", atom_to_list(Virture#vt_sql.table), "(", Fields, PrivateKey, Index, ")ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"],
%%        io:format("~s~n", [Sql]),
        case query(Sql) of
            {error, {1050, _, _}} ->% 表已经存在
                {Virture#vt_sql.table, exists};
            {error, Error} ->
                {Virture#vt_sql.table, Error};
            _ ->
                {Virture#vt_sql.table, ok}
        end
              end, VirtureList).

convert_type(?VT_INT32) ->
    "int";
convert_type(?VT_UINT32) ->
    "int unsigned";
convert_type(?VT_INT64) ->
    "bigint";
convert_type(?VT_UINT64) ->
    "bigint unsigned";
convert_type(?VT_FLOAT) ->
    "float";
convert_type(?VT_STRING) ->
    "text";
convert_type(?VT_TO_STRING) ->
    "text";
convert_type(?VT_BINARY) ->
    "blob";
convert_type(?VT_TO_BINARY) ->
    "blob";
convert_type(?VT_JSON_LIST) ->
    "json";
convert_type(?VT_JSON_OBJ(_)) ->
    "json";
convert_type(?VT_JSON_OBJ_LIST(_)) ->
    "json".

%% @private 初始化, system_init
-spec system_init() -> term().
system_init() ->
    system_init(undefined, undefined, undefined).

%% @private 如果没有数据需要修复Fun传undefined即可, 更多信息看 fix_dets/3
%%
%% 因为修复完dets数据就清空了, 所以fix_dets只需要执行一次, 跟版本没关系(对比fix_restart)
-spec system_init(undefined|function(), undefined|fun((Record :: tuple())-> term()), undefined|function()) -> term().
system_init(Before, Fun, After) ->
    ?LOG_NOTICE("~p", [build_table()]),
    file:make_dir(get_dets_path()),
    ?ETS_VT_SQL_LOAD = ets:new(?ETS_VT_SQL_LOAD, [public, named_table, {read_concurrency, true}]),
    lists:foreach(fun(Virture) ->
        case Virture#vt_sql.use_ets of
            true ->
                EtsName = make_ets_name(Virture),
                EtsName = ets:new(EtsName, [{keypos, Virture#vt_sql.private_key_pos} | lists:keydelete(keypos, 1, Virture#vt_sql.ets_opt)]);
            _ ->
                skip
        end
                  end, vt:all(mysql)),
    case fix_dets(Before, Fun, After) of
        ok -> ok;
        {error, Error} -> throw({virture, system_init, Error})
    end.

%% @private 保存定义
-spec save_defined() -> any().
save_defined() ->
    {ok, ?DETS_VT_SQL} = dets:open_file(?DETS_VT_SQL, [{file, get_dets_path() ++ "/" ++ atom_to_list(?DETS_VT_SQL)}, {keypos, #vt_sql.table}]),
    lists:foreach(fun(Virture) ->
        dets:insert(?DETS_VT_SQL, Virture)
                  end, vt:all(mysql)),
    dets:close(?DETS_VT_SQL).

%% @private 初始化进程字典
process_init() ->
    case get(?PD_VT_SQL) of
        undefined -> put(?PD_VT_SQL, #{});
        _ -> skip
    end,
    case get(?PD_VT_SQL_NOT_FLUSH) of
        undefined -> put(?PD_VT_SQL_NOT_FLUSH, []);
        _ -> skip
    end.


%% @doc 是否已加载某个表到ets
is_load_ets(Table, SelectKey) ->
    ets:member(?ETS_VT_SQL_LOAD, {Table, SelectKey}).

%% @doc 是否已加载某个表到ets, 没加载则加载
ensure_load_ets(Table, SelectKey) ->
    case ets:member(?ETS_VT_SQL_LOAD, {Table, SelectKey}) of
        true -> true;
        _ -> load(Table, SelectKey)
    end.

%% @equiv load(Table, Key, undefined)
load(Table, SelectKey) ->
    load(Table, SelectKey, undefined).

%% @doc 在当前进程初始化一个表
%%
%% 当SelectKey或WhereSql为undefined时, 等效于没有这个where条件
%%
%% WhereSql为额外的where语句, 你需要清楚自己在做什么, 否则请加载到内存进行复杂查询
%%
%% WhereSql=/=undefined时总是查询数据库再和本地数据比较, 同时不会缓存ETS_VT_SQL_LOAD
-spec load(atom(), undefiend|list(), undefined|iolist()) -> term().
load(Virture = #vt_sql{}, SelectKey, WhereSql) ->
    %% 初始化缓存
    Virture1 = init_virture(Virture),
    %% 初始化数据
    Virture2 = init_data(SelectKey, WhereSql, Virture1),
    VtSql = get(?PD_VT_SQL),
    put(?PD_VT_SQL, VtSql#{Virture#vt_sql.table => Virture2});
load(Table, SelectKey, WhereSql) ->
    Virture = vt:get(mysql, Table),
    load(Virture, SelectKey, WhereSql).


%% 查询一条数据
-spec lookup(atom(), list()) -> Record :: tuple()|undefined.
lookup(Table, Key) when is_list(Key) ->
    case get(?PD_VT_SQL) of
        #{Table := #vt_sql{state_pos = StatePos, data = Data}} ->
            case maps:get(Key, Data, undefined) of
                undefined ->
                    undefined;
                Record ->
                    %% 判断是否已删除
                    case element(StatePos, Record) of
                        ?VT_STATE_DELETE ->
                            undefined;
                        _ ->
                            Record
                    end
            end;
        _ ->% 没加载
            undefined
    end.


%% @doc 脏读ets, 上层需要保证数据已经加载
-spec lookup_ets(atom(), list()) -> undefined|tuple().
lookup_ets(Table, Key) ->
    case ets:lookup(make_ets_name(Table), Key) of
        [Record] ->
            Record;
        _ ->
            undefined
    end.

%% @doc 插入一条数据
-spec insert(tuple()) -> term().
insert(Record) ->
    Table = element(1, Record),
    #{Table := #vt_sql{
        use_ets = UseEts,
        state_pos = StatePos, private_key_pos = PrivateKeyPos,
        private_key = PrivateKey, data = Data, change = ChangeMap
    } = Virture} = VtSql = get(?PD_VT_SQL),
    Record1 = setelement(StatePos, Record, ?VT_STATE_REPLACE),
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
    case UseEts of
        true ->
            ChangeMap1 = ChangeMap#{Key => ?VT_STATE_REPLACE},
            Virture1 = Virture#vt_sql{has_change = true, data = Data1, change = ChangeMap1},
            put(?PD_VT_SQL, VtSql#{Table => Virture1}),
            save_not_flush(Table);
        _ ->
            Virture1 = Virture#vt_sql{has_change = true, data = Data1},
            put(?PD_VT_SQL, VtSql#{Table => Virture1})
    end.

%% @doc 删除一条数据
-spec delete(atom(), list()) -> term().
delete(Table, Key) when is_list(Key) ->
    #{Table := #vt_sql{
        use_ets = UseEts,
        state_pos = StatePos, private_key_pos = PrivateKeyPos,
        data = Data, change = ChangeMap
    } = Virture} = VtSql = get(?PD_VT_SQL),
    %% 构造record, 不需要全部的record字段, 关键数据在就可以
    Record = erlang:make_tuple(max(StatePos, PrivateKeyPos), undefined, [{1, Table}, {PrivateKeyPos, Key}, {StatePos, ?VT_STATE_DELETE}]),
    %% 更新缓存
    Data1 = Data#{Key => Record},
    %% 更新change
    case UseEts of
        true ->
            ChangeMap1 = ChangeMap#{Key => ?VT_STATE_DELETE},
            Virture1 = Virture#vt_sql{has_change = true, data = Data1, change = ChangeMap1},
            put(?PD_VT_SQL, VtSql#{Table => Virture1}),
            save_not_flush(Table);
        _ ->
            Virture1 = Virture#vt_sql{has_change = true, data = Data1},
            put(?PD_VT_SQL, VtSql#{Table => Virture1})
    end.


%% 记录改变的table
save_not_flush(Table) ->
    NotFlush = get(?PD_VT_SQL_NOT_FLUSH),
    case lists:member(Table, NotFlush) of
        true -> skip;
        _ -> put(?PD_VT_SQL_NOT_FLUSH, [Table | NotFlush])
    end.


%% @doc 遍历当前数据, 支持break
-spec fold_cache(fun((Key :: term(), Record :: term(), Acc :: term())-> Acc1 :: term()), term(), atom()) -> term().
fold_cache(Fun, Acc, Table) ->
    #{Table := #vt_sql{state_pos = StatePos, data = Data}} = get(?PD_VT_SQL),
    fold_cache_1(Fun, Acc, StatePos, Data).

%% 从maps复制
fold_cache_1(Fun, Init, StatePos, Map) when is_function(Fun, 3), is_map(Map) ->
    fold_cache_2(Fun, Init, StatePos, maps:iterator(Map)).

fold_cache_2(Fun, Acc, StatePos, Iter) ->
    case maps:next(Iter) of
        {K, V, NextIter} ->
            case element(StatePos, V) of
                ?VT_STATE_DELETE ->
                    fold_cache_2(Fun, Acc, StatePos, NextIter);
                _ ->
                    case Fun(K, V, Acc) of
                        ?FOLD_BREAK -> Acc;
                        ?FOLD_BREAK1(Acc1) -> Acc1;
                        Acc1 -> fold_cache_2(Fun, Acc1, StatePos, NextIter)
                    end
            end;
        none ->
            Acc
    end.

%% @doc flush所有改变的数据到ets
-spec sync_to_ets() -> ok.
sync_to_ets() ->
    case get(?PD_VT_SQL_NOT_FLUSH) of
        [] -> ok;
        NotFlush ->
            sync_to_ets(NotFlush, get(?PD_VT_SQL)),
            put(?PD_VT_SQL_NOT_FLUSH, [])
    end.

sync_to_ets([], VtSql) ->
    put(?PD_VT_SQL, VtSql);
sync_to_ets([H | T], VtSql) ->
    Virture = maps:get(H, VtSql),
    Virture1 = do_sync_to_ets(Virture),
    VtSql1 = VtSql#{H => Virture1},
    sync_to_ets(T, VtSql1).

do_sync_to_ets(#vt_sql{ets = Ets, use_ets = true, data = Data, change = Change} = Virture) ->
    maps:fold(fun(K, S, _Acc) ->
        case S of
            ?VT_STATE_DELETE ->
                ets:delete(Ets, K);
            ?VT_STATE_REPLACE ->
                #{K := Record} = Data,
                ets:insert(Ets, Record)
        end
              end, [], Change),
    Virture#vt_sql{change = #{}};
do_sync_to_ets(Virture) ->
    ?LOG_WARNING("~p", [Virture#vt_sql.ets]),
    Virture.

%% @doc 同步到数据库, 同时也会同步到ets, 若同步数据库失败, 则同步到dets
sync_to_db() ->
    VtSql = get(?PD_VT_SQL),
    %% 遍历所有数据
    VtSql1 =
        maps:fold(fun(Table, #vt_sql{has_change = true} = Virture, VM) ->
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
                  end, VtSql, VtSql),
    put(?PD_VT_SQL, VtSql1).

sync_to_db(#vt_sql{
    ets = Ets, use_ets = UseEts,
    state_pos = StatePos, data = Data, private_key = PrivateKey, all_fields = AllFields,
    private_where_sql = WhereSql, replace_sql = ReplaceSql, delete_sql = DeleteSql
} = Virture) ->
    %% 更新数据, 拼sql
    {Data1, ReplaceIOList, DeleteIOList, _, LastData, HasBadData} =
        maps:fold(fun(Key, Record, {D, RIO, DIO, N, LD, HBD} = Acc) ->
            try
                case element(StatePos, Record) of
                    ?VT_STATE_REPLACE ->
                        ?DO_IF(UseEts, ets:insert(Ets, Record)),
                        Record1 = setelement(StatePos, Record, ?VT_STATE_NOT_CHANGE),
                        D1 = D#{Key => Record1},
                        [[_, First] | Left] = [[$,, encode(Type, element(Pos, Record))] || #vt_sql_field{type = Type, pos = Pos} <- AllFields],
                        RIO1 = [$,, $(, First, Left, $) | RIO],
                        sync_db_check_sql_limit(ReplaceSql, RIO1, DIO, D1, N + 1, LD, HBD);
                    ?VT_STATE_DELETE ->
                        ?DO_IF(UseEts, ets:delete(Ets, Key)),
                        D1 = maps:remove(Key, D),
                        WhereSql1 = join_where(WhereSql, PrivateKey, Key),
                        DIO1 = [DeleteSql, WhereSql1, $; | DIO],
                        sync_db_check_sql_limit(ReplaceSql, RIO, DIO1, D1, N + 1, LD, HBD);
                    ?VT_STATE_NOT_CHANGE ->
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
                Virture#vt_sql{has_change = true, data = LastData, change = #{}};
            _ ->
                HasBadData1 = HasBadData,
                Virture#vt_sql{has_change = false, data = Data1, change = #{}}
        end,
    {HasBadData1, Virture1}.


%% 防止sql过长
sync_db_check_sql_limit(ReplaceSql, ReplaceIOList, DeleteIOList, Data, N, LastData, HasBadData) ->
    case N >= ?VT_SQL_SQL_LIMIT andalso do_sync_to_db(ReplaceSql, ReplaceIOList, DeleteIOList) of
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
            query([ReplaceSql, ReplaceIOList1, $;, DeleteIOList]);
        DeleteIOList =/= [] ->
            query(DeleteIOList);
        true ->
            ok
    end.

%% mysql失败的数据保存到dets
%% 因为失败的是一个个数据段, 所以保存的内容也可能有对有错
sync_dets(#vt_sql{table = Table, data = Data, private_key_pos = PKPos}) ->
    case dets:open_file(Table, [{file, get_dets_path() ++ "/" ++ atom_to_list(Table)}, {keypos, PKPos}]) of
        {ok, Table} ->
            maps:fold(fun(_K, V, _) ->
                dets:insert(Table, V)
                      end, [], Data),
            dets:close(Table);
        Error ->
            %% 这里都失败的话没救了
            ?LOG_ERROR("~p", [Error])
    end.

%% @doc 检查dets是否存在数据
check_dets() ->
    DetsPath = get_dets_path(),
    {ok, ?DETS_VT_SQL} = dets:open_file(?DETS_VT_SQL, [{file, DetsPath ++ "/" ++ atom_to_list(?DETS_VT_SQL)}, {keypos, #vt_sql.table}]),
    filelib:fold_files(DetsPath, ".*", false,
        fun(FileName, _Acc) ->
            Table = list_to_atom(filename:basename(FileName)),
            case Table of
                ?DETS_VT_SQL ->
                    skip;
                _ ->
                    [Virture] = dets:lookup(?DETS_VT_SQL, Table),
                    case dets:open_file(Table, [{file, FileName}, {keypos, Virture#vt_sql.private_key_pos}]) of
                        {ok, Table} ->
                            case dets:info(Table, size) > 0 of
                                true ->
                                    throw({vt_sql, dets, noempty, Table});
                                _ ->
                                    dets:close(Table),
                                    file:delete(FileName)
                            end;
                        Error ->
                            throw({vt_sql, dets, error, Error})
                    end
            end
        end, []).

%% @doc 修复dets数据, 同步到数据库
%%
%% 提供两个钩子, 全部修复开始前, 全部修复成功后
%%
%% Fun函数里面必须包含insert或者delete操作, 否则数据不会落地
%%
%% 如果数据已删除, 则函数传入{delete, key}
%%
%% 若vt_sql定义变了且需要使用新的定义, 可以通过hotfix先覆盖定义再进行修复
%%
%% 简单的修复, A数据在dets, 仅使用正常的数据+坏掉数据A修复
%%
%% 如果涉及复杂修复, 请直接使用dets和sql修复, 因为这里无法处理坏掉数据A+坏掉数据B交叉
-spec fix_dets(undefined|function(), fun((Record :: tuple()|{delete, Key :: list()})-> term()), undefined|function()) -> term().
fix_dets(Before, Fun, After) ->
    Self = self(),
    OldTrapExit = erlang:process_flag(trap_exit, true),
    %% virtue使用进程字典, 使用新进程防止数据混淆
    Spawn =
        spawn_link(fun() ->
            process_init(),
            DetsPath = get_dets_path(),
            {ok, ?DETS_VT_SQL} = dets:open_file(?DETS_VT_SQL, [{file, DetsPath ++ "/" ++ atom_to_list(?DETS_VT_SQL)}, {keypos, #vt_sql.table}]),
            ?DO_IF(is_function(Before), Before()),
            filelib:fold_files(DetsPath, ".*", false,
                fun(FileName, _Acc) ->
                    Table = list_to_atom(filename:basename(FileName)),
                    case Table of
                        ?DETS_VT_SQL ->
                            skip;
                        _ ->
                            [#vt_sql{state_pos = StatePos, private_key_pos = PKPos} = Virture] = dets:lookup(?DETS_VT_SQL, Table),
                            case dets:open_file(Table, [{file, FileName}, {keypos, PKPos}]) of
                                {ok, Table} ->
                                    %% 兼容在线修复
                                    ok = dets:sync(Table),
                                    %% 拿到全部数据
                                    AllRecord = dets:select(Table, [{'_', [], ['$_']}]),
                                    %% 初始化一个空的Virture
                                    Virture1 = init_virture(Virture),
                                    VtSql = get(?PD_VT_SQL),
                                    put(?PD_VT_SQL, VtSql#{Table => Virture1}),
                                    WarpFun = fun(R) ->
                                        case element(StatePos, R) of
                                            ?VT_STATE_DELETE -> Fun({delete, element(PKPos, R)});
                                            _ -> Fun(R)
                                        end end,
                                    lists:foreach(WarpFun, AllRecord),
                                    %% 同步到数据库
                                    #{Table := Virture2} = get(?PD_VT_SQL),
                                    case sync_to_db(Virture2) of
                                        {true, _} ->% 还是有错误, 中断
                                            throw({vt_sql, dets, db, fail});
                                        {_, Virture3} ->
                                            VtSql1 = get(?PD_VT_SQL),
                                            put(?PD_VT_SQL, VtSql1#{Table => Virture3}),
                                            %% 删除dets
                                            dets:close(Table),
                                            file:delete(FileName)
                                    end;
                                Error ->
                                    throw({vt_sql, dets, error, Error})
                            end
                    end
                end, []),
            dets:close(?DETS_VT_SQL),
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

%% @doc 获取当前状态, 用于rollback/1
-spec hold() -> {list(), map()}.
hold() ->
    {get(?PD_VT_SQL_NOT_FLUSH), get(?PD_VT_SQL)}.

%% @doc hold/0的状态回滚
-spec rollback({list(), map()}) -> term().
rollback({NotFlush, VtSql}) ->
    put(?PD_VT_SQL_NOT_FLUSH, NotFlush),
    put(?PD_VT_SQL, VtSql).

%% @doc 清理进程字典缓存数据
clean_pd() ->
    put(?PD_VT_SQL, #{}),
    put(?PD_VT_SQL_NOT_FLUSH, []).

%% @doc 清理某些table的进程字典缓存数据
-spec clean_pd(list()) -> term().
clean_pd(TableList) ->
    clean(TableList, get(?PD_VT_SQL), get(?PD_VT_SQL_NOT_FLUSH)).

clean([], VtSql, NotFlush) ->
    put(?PD_VT_SQL, VtSql),
    put(?PD_VT_SQL_NOT_FLUSH, NotFlush),
    ok;
clean([Table | T], VtSql, NotFlush) ->
    clean(T, maps:remove(Table, VtSql), lists:delete(Table, NotFlush)).

%% @doc 清理所有ets数据
-spec clean_ets() -> any().
clean_ets() ->
    TableList = lists:usort(ets:select(?ETS_VT_SQL_LOAD, [{{{'$1', '_'}}, [], ['$1']}])),
    lists:foreach(fun(Table) ->
        ets:delete_all_objects(vt_sql:make_ets_name(Table))
                  end, TableList),
    ets:delete_all_objects(?ETS_LOCAL_LOCK).

%% @doc 清理某些table的ets数据
-spec clean_ets(list()) -> list().
clean_ets([]) ->
    [];
clean_ets([Table | T]) ->
    ets:delete_all_objects(vt_sql:make_ets_name(Table)),
    ets:select_delete(?ETS_VT_SQL_LOAD, [{{{Table, '_'}}, [], [true]}]),
    clean_ets(T).

%% @doc 获取全部进程初始化的table
-spec all_table() -> list().
all_table() ->
    case get(?PD_VT_SQL) of
        VtSql when is_map(VtSql) -> maps:keys(VtSql);
        _ -> []
    end.

%% @doc 热修复进程中的某个表
%%
%% 如果定义变了(is_diff_def/2) ,Fun函数里面必须包含insert或者delete操作, 否则数据会错乱
%%
%% 如果数据已删除, 则函数传入{delete, key}
%%
%% 另外ets不能作任何更改, 因为实际上不太需要这么做, 所以相应的private_key_pos也不能改变
%%
%% 简单的修复, A数据在缓存, 仅使用正常的数据+坏掉数据A修复
%%
%% 如果涉及复杂修复, 请直接使用fold_cache和ets和sql修复, 因为这里无法处理坏掉数据A+坏掉数据B交叉
-spec hotfix(atom(), fun((Record :: tuple()|{delete, Key :: list()}) -> term())) -> any().
hotfix(Table, Fun) ->
    Hold = hold(),
    #{Table := Virture} = VtSql = get(?PD_VT_SQL),
    case catch do_hotfix(Fun, Virture, VtSql) of
        ok ->
            ok;
        Error ->
            ?LOG_ERROR("hotfix~n~p", [Error]),
            rollback(Hold),
            error
    end.

do_hotfix(Fun, #vt_sql{table = Table, use_ets = UseEts, state_pos = StatePos, private_key_pos = PKPos, data = Data} = OldVirture, VtSql) ->
    {ok, ?DETS_VT_SQL} = dets:open_file(?DETS_VT_SQL, [{file, get_dets_path() ++ "/" ++ atom_to_list(?DETS_VT_SQL)}, {keypos, #vt_sql.table}]),
    case vt:get(mysql, Table) of
        #vt_sql{} = NewConfig ->
            case UseEts == NewConfig#vt_sql.use_ets andalso PKPos == NewConfig#vt_sql.private_key_pos of
                true -> skip;
                _ ->
                    %% ets无法中途变更
                    throw({vt_sql, Table, config_cant_change})
            end,
            NewVirture =
                case is_same_def(OldVirture, NewConfig) of
                    true ->% 数据定义没改变, 杂项更新
                        OldVirture#vt_sql{
                            init_fun = NewConfig#vt_sql.init_fun
                        };
                    _ ->
                        %% 初始化新的
                        NewVirture_1 = init_virture(NewConfig),
                        %% 老数据赋值回去
                        NewVirture_1#vt_sql{
                            data = Data,
                            change = OldVirture#vt_sql.change,
                            has_change = OldVirture#vt_sql.has_change
                        }
                end,
            VMsql1 = VtSql#{Table => NewVirture},
            put(?PD_VT_SQL, VMsql1),
            %% 执行Fun
            WarpFun = fun(K, R, _Acc) ->
                case element(StatePos, R) of
                    ?VT_STATE_DELETE -> Fun({delete, K});
                    _ -> Fun(R)
                end end,
            maps:fold(WarpFun, [], Data),
            %% 全部成功, 更新定义
            dets:insert(?DETS_VT_SQL, NewConfig),
            dets:close(?DETS_VT_SQL);
        _ ->% 表被删除了
            VMsql1 = maps:remove(Table, VtSql),
            put(?PD_VT_SQL, VMsql1),
            put(?PD_VT_SQL_NOT_FLUSH, lists:delete(Table, get(?PD_VT_SQL_NOT_FLUSH))),
            dets:delete(?DETS_VT_SQL, Table),
            dets:close(?DETS_VT_SQL)
    end,
    ok.

%% 定义是否改变
is_same_def(Old, New) ->
    Old#vt_sql.state_pos == New#vt_sql.state_pos
        andalso Old#vt_sql.select_key == New#vt_sql.select_key
        andalso Old#vt_sql.private_key == New#vt_sql.private_key
        andalso Old#vt_sql.all_fields == New#vt_sql.all_fields
        andalso Old#vt_sql.record_size == New#vt_sql.record_size.

%% @doc 数据库查询
query(IOList) ->
    case mysql_poolboy:query(?VT_SQL_POOL, IOList) of
        {error, Error} = E ->
            ?LOG_WARNING("~p", [Error]),
            E;
        Ok ->
            Ok
    end.

%% 初始化
init_virture(#vt_sql{all_fields = AllField, private_key = PrivateKey, select_key = Selectkey} = Virture) ->
    %% 初始化优化sql
    Virture#vt_sql{
        ets = make_ets_name(Virture),
        private_key = [lists:keyfind(Field, #vt_sql_field.name, AllField) || Field <- PrivateKey],
        select_key = [lists:keyfind(Field, #vt_sql_field.name, AllField) || Field <- Selectkey],
        private_where_sql = make_private_where_sql(Virture),
        select_where_sql = make_select_where_sql(Virture),
        select_sql = make_select_sql(Virture),
        replace_sql = make_replace_sql(Virture),
        delete_sql = make_delete_sql(Virture)
    }.

%% ets名字
make_ets_name(#vt_sql{table = Table}) ->
    make_ets_name(Table);
make_ets_name(Table) ->
    list_to_atom("ets_" ++ atom_to_list(Table)).

make_private_where_sql(Virture) ->
    case Virture#vt_sql.private_key of
        [H] ->
            [" WHERE " ++ atom_to_list(H) ++ "="];
        [H | T] ->
            HSql = " WHERE " ++ atom_to_list(H) ++ "=",
            TSql = [" AND " ++ atom_to_list(Name) ++ "=" || Name <- T],
            [HSql | TSql]
    end.

make_select_where_sql(Virture) ->
    case Virture#vt_sql.select_key of
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
    AllField = string:join([atom_to_list(VK#vt_sql_field.name) || VK <- Virture#vt_sql.all_fields], ","),
    "SELECT " ++ AllField ++ " FROM " ++ atom_to_list(Virture#vt_sql.table).

make_replace_sql(Virture) ->
    AllField = string:join([atom_to_list(VK#vt_sql_field.name) || VK <- Virture#vt_sql.all_fields], ","),
    ["REPLACE INTO " ++ atom_to_list(Virture#vt_sql.table) ++ " (" ++ AllField ++ ")VALUES"].

make_delete_sql(Virture) ->
    "DELETE FROM " ++ atom_to_list(Virture#vt_sql.table).

%% 拼where的sql
join_where([], [], []) ->
    [];
join_where([SqlH | SqlT], [#vt_sql_field{type = Type} | FieldT], [Value | ValueT]) ->
    [SqlH, encode(Type, Value) | join_where(SqlT, FieldT, ValueT)].

%% 初始化数据
init_data(SelectValue, ExtWhereSql, Virture) ->
    #vt_sql{
        table = Table, use_ets = UseEts, ets = EtsName,
        private_key_pos = PrivateKeyPos, state_pos = StatePos,
        private_key = PrivateKey, select_key = SelectKey,
        select_where_sql = WhereSql, select_sql = SelectSql,
        record_size = RecordSize,
        data = VData, init_fun = InitFun
    } = Virture,
    case UseEts == false orelse ExtWhereSql =/= undefined orelse SelectValue == undefined orelse is_load_ets(Table, SelectValue) == false of
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
            {ok, _ColumnNames, Rows} = query([SelectSql, WhereSql1]),
            %% 构造record数据
            Data =
                lists:foldl(fun(Row, Acc) ->
                    Record = init_record(InitFun, Row, Virture),
                    %% 构造key
                    K = make_private_key(PrivateKey, Record),
                    Record1 = setelement(PrivateKeyPos, Record, K),
                    %% 设置状态
                    Record2 = erlang:setelement(StatePos, Record1, ?VT_STATE_NOT_CHANGE),
                    Acc#{K => Record2}
                            end, VData, Rows),
            Data1 =
                case UseEts of
                    true ->
                        %% 全部初始化成功再放到ets
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
                                  end, Data, Data);
                    _ ->
                        Data
                end,
            %% where总是查表
            ?DO_IF(UseEts andalso WhereSql =/= undefined, ets:insert(?ETS_VT_SQL_LOAD, {{Table, SelectValue}})),
            Virture#vt_sql{data = Data1};
        _ ->
            %% 从ets拿
            Spec = erlang:make_tuple(RecordSize, '_'),
            KeySpec = make_ets_key_spec(PrivateKey, SelectKey, SelectValue),
            Spec1 = setelement(PrivateKeyPos, Spec, KeySpec),
            EtsData = ets:select(EtsName, [{Spec1, [], ['$_']}]),
            %% 构造本地缓存
            Data =
                lists:foldl(fun(Record, Acc) ->
                    %% flush时不会重新设置state
                    Acc#{element(PrivateKeyPos, Record) => setelement(StatePos, Record, ?VT_STATE_NOT_CHANGE)}
                            end, VData, EtsData),
            Virture#vt_sql{data = Data}
    end.


make_private_key(PrivateKey, Record) ->
    [element(Pos, Record) || #vt_sql_field{pos = Pos} <- PrivateKey].


make_ets_key_spec([], _SelectKey, _Value) ->
    [];
make_ets_key_spec([#vt_sql_field{pos = Pos} | PKT], SelectKey, ValueList) ->
    case make_ets_key_spec_find_value(Pos, SelectKey, ValueList) of
        false ->
            ['_' | make_ets_key_spec(PKT, SelectKey, ValueList)];
        Value ->
            [Value | make_ets_key_spec(PKT, SelectKey, ValueList)]
    end.

make_ets_key_spec_find_value(_Pos, [], []) ->
    false;
make_ets_key_spec_find_value(Pos, [#vt_sql_field{pos = Pos} | _FT], [Value | _VT]) ->
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
encode(?VT_JSON_LIST, List) ->
    encode_json_list(List);
encode(?VT_JSON_OBJ(NameList), Value) ->
    encode_json_obj(NameList, Value);
encode(?VT_JSON_OBJ_LIST(NameList), Value) ->
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

do_encode_json_single_obj(?VT_JSON_LIST(Name), V) ->
    [$', Name, $', $,, encode_json_list(V)];
do_encode_json_single_obj(?VT_JSON_OBJ(Name, NameList), V) ->
    [$', Name, $', $,, encode_json_obj(NameList, V)];
do_encode_json_single_obj(?VT_JSON_OBJ_LIST(Name, NameList), V) ->
    [$', Name, $', $,, encode_json_obj_list(NameList, V)];
do_encode_json_single_obj(Name, V) ->
    [$', Name, $', $,, mysql_encode:encode(V)].


decode(to_string, Value) ->
    util:eval(Value);
decode(to_binary, Value) ->
    erlang:binary_to_term(Value);
decode(?VT_JSON_LIST, Value) ->
    jsx:decode(Value);
decode(?VT_JSON_OBJ(NameList), Value) ->
    decode_json_obj(NameList, jsx:decode(Value));
decode(?VT_JSON_OBJ_LIST(NameList), Value) ->
    decode_json_obj_list(NameList, jsx:decode(Value));
decode(_, Value) ->
    Value.

decode_json_obj(NameList, Map) ->
    list_to_tuple([decode_json_single_obj(Name, Map) || Name <- NameList]).

decode_json_obj_list(NameList, List) ->
    [decode_json_obj(NameList, Obj) || Obj <- List].

decode_json_single_obj(?VT_JSON_LIST(Name), Map) ->
    jsx:decode(maps:get(Name, Map));
decode_json_single_obj(?VT_JSON_OBJ(Name, NameList), Map) ->
    decode_json_obj(NameList, maps:get(Name, Map));
decode_json_single_obj(?VT_JSON_OBJ_LIST(Name, NameList), Map) ->
    decode_json_obj_list(NameList, maps:get(Name, Map));
decode_json_single_obj(Name, Map) ->
    maps:get(Name, Map).

%% 初始化record
init_record(Init, Row, Virture) ->
    BaseRecord = erlang:make_tuple(Virture#vt_sql.record_size, undefined, [{1, Virture#vt_sql.table}]),
    init_record(Init, Row, Virture#vt_sql.all_fields, BaseRecord).

init_record(InitFun, [], [], Record) ->
    case InitFun of
        {M, F} -> M:F(Record);
        _ -> Record
    end;
init_record(InitFun, [Value | ValueT], [#vt_sql_field{pos = Pos, type = Type} | FieldT], Record) ->
    init_record(InitFun, ValueT, FieldT, setelement(Pos, Record, decode(Type, Value))).

-ifdef(TEST).
get_dets_path() ->
    "vt_sql_dets".
-else.
get_dets_path() ->
    {ok, Config} = application:get_env(ptolemaios, virture),
    {_, Path} = lists:keyfind(mysql_dets, 1, Config),
    Path.
-endif.

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
            mysql_poolboy:add_pool(?VT_SQL_POOL, PoolOptions, MySqlOptions),
            %% 删除test数据表
            mysql_poolboy:query(?VT_SQL_POOL, "drop table if exists vt_sql_test_player;
    drop table if exists vt_sql_test_goods;
    drop table if exists vt_sql_test_equip;"),
            %% build
            vt_sql:system_init()
        end,
        fun(_) ->
            application:stop(mysql_poolboy),
            application:stop(poolboy),
            application:stop(mysql)
        end,
        fun(_) ->
            %% 懒得写那么多宏了
            %% 初始化
            vt_sql:process_init(),
            vt_sql:load(vt_sql_test_player, [1]),
            vt_sql:load(vt_sql_test_goods, [1]),
            %% 插入数据
            undefined = vt_sql:lookup(vt_sql_test_player, [1]),
            undefined = vt_sql:lookup(vt_sql_test_goods, [1, 1]),
            vt_sql:insert(#vt_sql_test_player{player_id = 1, str = <<"1">>, to_str = <<"1">>, to_bin = <<"1">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vt_sql:insert(#vt_sql_test_player{player_id = 2, str = <<"2">>, to_str = <<"2">>, to_bin = <<"2">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vt_sql:insert(#vt_sql_test_goods{player_id = 1, goods_id = 1, str = <<"1">>, to_str = <<"1">>, to_bin = <<"1">>}),
            vt_sql:insert(#vt_sql_test_goods{player_id = 1, goods_id = 2, str = <<"2">>, to_str = <<"2">>, to_bin = <<"2">>}),
            vt_sql:insert(#vt_sql_test_goods{player_id = 2, goods_id = 1, str = <<"1">>, to_str = <<"1">>, to_bin = <<"1">>}),
            vt_sql:insert(#vt_sql_test_goods{player_id = 2, goods_id = 2, str = <<"2">>, to_str = <<"2">>, to_bin = <<"2">>}),
            0 = ets:info(vt_sql:make_ets_name(vt_sql_test_player), size),
            0 = ets:info(vt_sql:make_ets_name(vt_sql_test_goods), size),
            vt_sql:sync_to_db(),
            2 = ets:info(vt_sql:make_ets_name(vt_sql_test_player), size),
            4 = ets:info(vt_sql:make_ets_name(vt_sql_test_goods), size),
%%    ?debugFmt("~p~n", [get()]),
            %% 插入+更新+删除
            vt_sql:insert(#vt_sql_test_player{player_id = 1, str = <<"11">>, to_str = <<"11">>, to_bin = <<"11">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vt_sql:delete(vt_sql_test_player, [2]),
            vt_sql:insert(#vt_sql_test_goods{player_id = 1, goods_id = 1, str = <<"11">>, to_str = <<"11">>, to_bin = <<"11">>}),
            vt_sql:delete(vt_sql_test_goods, [1, 2]),
            vt_sql:insert(#vt_sql_test_player{player_id = 3, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vt_sql:insert(#vt_sql_test_goods{player_id = 3, goods_id = 1, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>}),
            vt_sql:insert(#vt_sql_test_goods{player_id = 3, goods_id = 2, str = <<"3">>, to_str = <<"3">>, to_bin = <<"3">>}),
            #vt_sql_test_player{str = <<"11">>} = vt_sql:lookup(vt_sql_test_player, [1]),
            undefined = vt_sql:lookup(vt_sql_test_player, [2]),
            #vt_sql_test_player{str = <<"3">>} = vt_sql:lookup(vt_sql_test_player, [3]),
            #vt_sql_test_goods{str = <<"11">>} = vt_sql:lookup(vt_sql_test_goods, [1, 1]),
            undefined = vt_sql:lookup(vt_sql_test_goods, [1, 2]),
            #vt_sql_test_goods{str = <<"3">>} = vt_sql:lookup(vt_sql_test_goods, [3, 1]),
%%    ?debugFmt("~p~n", [get()]),
            vt_sql:sync_to_db(),
            2 = ets:info(vt_sql:make_ets_name(vt_sql_test_player), size),
            5 = ets:info(vt_sql:make_ets_name(vt_sql_test_goods), size),
            #vt_sql_test_player{str = <<"11">>} = vt_sql:lookup(vt_sql_test_player, [1]),
            undefined = vt_sql:lookup(vt_sql_test_player, [2]),
            #vt_sql_test_player{str = <<"3">>} = vt_sql:lookup(vt_sql_test_player, [3]),
            #vt_sql_test_goods{str = <<"11">>} = vt_sql:lookup(vt_sql_test_goods, [1, 1]),
            undefined = vt_sql:lookup(vt_sql_test_goods, [1, 2]),
            #vt_sql_test_goods{str = <<"3">>} = vt_sql:lookup(vt_sql_test_goods, [3, 1]),
            %% hold
            Hold = vt_sql:hold(),
            vt_sql:insert(#vt_sql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vt_sql:insert(#vt_sql_test_goods{player_id = 4, goods_id = 1, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            vt_sql:insert(#vt_sql_test_goods{player_id = 4, goods_id = 2, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            #vt_sql_test_player{} = vt_sql:lookup(vt_sql_test_player, [4]),
            #vt_sql_test_goods{} = vt_sql:lookup(vt_sql_test_goods, [4, 1]),
            #vt_sql_test_goods{} = vt_sql:lookup(vt_sql_test_goods, [4, 2]),
            vt_sql:rollback(Hold),
            2 = ets:info(vt_sql:make_ets_name(vt_sql_test_player), size),
            5 = ets:info(vt_sql:make_ets_name(vt_sql_test_goods), size),
            undefined = vt_sql:lookup(vt_sql_test_player, [4]),
            undefined = vt_sql:lookup(vt_sql_test_goods, [4, 1]),
            undefined = vt_sql:lookup(vt_sql_test_goods, [4, 2]),
            %% all_table
            [] = vt_sql:all_table()--[vt_sql_test_player, vt_sql_test_goods],
            vt_sql:clean_pd(vt_sql:all_table()),
            [] = vt_sql:all_table(),
            %% sync
            vt_sql:load(vt_sql_test_player, [1]),
            vt_sql:load(vt_sql_test_goods, [1]),
            vt_sql:insert(#vt_sql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>, to_json = [{1, {2, 3}}, {11, {22, 33}}]}),
            vt_sql:insert(#vt_sql_test_goods{player_id = 4, goods_id = 1, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            vt_sql:insert(#vt_sql_test_goods{player_id = 4, goods_id = 2, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            2 = ets:info(vt_sql:make_ets_name(vt_sql_test_player), size),
            5 = ets:info(vt_sql:make_ets_name(vt_sql_test_goods), size),
            sync_to_db(maps:get(vt_sql_test_goods, get(?PD_VT_SQL))),
            2 = ets:info(vt_sql:make_ets_name(vt_sql_test_player), size),
            7 = ets:info(vt_sql:make_ets_name(vt_sql_test_goods), size),
            sync_to_db(maps:get(vt_sql_test_goods, get(?PD_VT_SQL))),
            2 = ets:info(vt_sql:make_ets_name(vt_sql_test_player), size),
            7 = ets:info(vt_sql:make_ets_name(vt_sql_test_goods), size),
            %% 初始化
            vt_sql:clean_ets([vt_sql_test_player]),
            vt_sql:clean_pd(),
            %% dets
            vt_sql:load(vt_sql_test_player, [4]),
            %% 没有json, sync会失败, 会有一个正常报错
            vt_sql:insert(#vt_sql_test_player{player_id = 4, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            sync_to_db(),
            vt_sql:clean_pd(),
            vt_sql:fix_dets(undefined, fun(R) ->
                R1 = R#vt_sql_test_player{to_json = [{1, {2, 3}}, {11, {22, 33}}]},
                vt_sql:insert(R1)
                                       end, undefined),
            [#vt_sql_test_player{}] = ets:lookup(vt_sql:make_ets_name(vt_sql_test_player), [4]),
            vt_sql:clean_pd([vt_sql_test_player]),
            vt_sql:clean_ets([vt_sql_test_player]),
            vt_sql:load(vt_sql_test_player, [4]),
            #vt_sql_test_player{} = vt_sql:lookup(vt_sql_test_player, [4]),
            %% dirty lookup
            vt_sql:clean_ets([vt_sql_test_goods]),
            vt_sql:clean_pd(),
            undefined = vt_sql:lookup_ets(vt_sql_test_goods, [4, 1]),
            vt_sql:load(vt_sql_test_goods, [4]),
            clean_pd(),
            #vt_sql_test_goods{} = vt_sql:lookup_ets(vt_sql_test_goods, [4, 1]),
            %% fold_cache
            vt_sql:load(vt_sql_test_goods, [4]),
            2 = fold_cache(fun(_K, _V, Acc) ->
                Acc + 1
                           end, 0, vt_sql_test_goods),
            vt_sql:insert(#vt_sql_test_goods{player_id = 4, goods_id = 3, str = <<"4">>, to_str = <<"4">>, to_bin = <<"4">>}),
            3 = fold_cache(fun(_K, _V, Acc) ->
                Acc + 1
                           end, 0, vt_sql_test_goods),
            %% 热更新定义, shell 执行
            %% vt_sql_test_goods的to_str改成string类型
            %% os:cmd("rebar3 compile"), rr("include/*"), l(vt_sql), l(virture_config).
            %% vt_sql:hotfix(vt_sql_test_goods, fun(R) -> vt_sql:insert(R#vt_sql_test_goods{to_str = <<"hotfix">>}) end).
            %% #vt_sql_test_goods{to_str = <<"hotfix">>} = vt_sql:lookup(vt_sql_test_goods, [4, 1]).
            %% vt_sql:sync().
            %% 看数据库
            %% vt_sql:clean(), vt_sql:clean_ets().
            %% vt_sql:load(vt_sql_test_goods, [4]), vt_sql:lookup(vt_sql_test_goods, [4, 1]).
            %% 无ets缓存
            undefined = ets:info(vt_sql:make_ets_name(vt_sql_test_equip)),
            vt_sql:load(vt_sql_test_equip, [1]),
            undefined = vt_sql:lookup(vt_sql_test_equip, [1, 1]),
            vt_sql:insert(#vt_sql_test_equip{player_id = 1, equip_id = 1}),
            #vt_sql_test_equip{player_id = 1, equip_id = 1} = vt_sql:lookup(vt_sql_test_equip, [1, 1]),
            vt_sql:sync_to_ets(),
            vt_sql:sync_to_db(),
            vt_sql:clean_pd(),
            vt_sql:load(vt_sql_test_equip, [1]),
            #vt_sql_test_equip{player_id = 1, equip_id = 1} = vt_sql:lookup(vt_sql_test_equip, [1, 1]),
            vt_sql:insert(#vt_sql_test_equip{player_id = 1, equip_id = 2}),
            vt_sql:insert(#vt_sql_test_equip{player_id = 1, equip_id = 3}),
            vt_sql:delete(vt_sql_test_equip, [1, 1]),
            vt_sql:sync_to_db(),
            vt_sql:clean_pd(),
            vt_sql:load(vt_sql_test_equip, [1]),
            undefined = vt_sql:lookup(vt_sql_test_equip, [1, 1]),
            #vt_sql_test_equip{player_id = 1, equip_id = 2} = vt_sql:lookup(vt_sql_test_equip, [1, 2]),
            #vt_sql_test_equip{player_id = 1, equip_id = 3} = vt_sql:lookup(vt_sql_test_equip, [1, 3]),
            []
        end}.

-endif.