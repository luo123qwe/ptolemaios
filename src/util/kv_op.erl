%%%-------------------------------------------------------------------
%%% @author dominic
%%% @doc
%%% 键值型数据结构操作
%%% @end
%%%-------------------------------------------------------------------
-module(kv_op).
-author("dominic").

-include("util.hrl").

%% 效率损耗, 判断数据类型和存储的时候会再次查找一遍
%% 主要解决深嵌套时的查询和更改
%% 支持 tuple_list, map, tuple, map, dict, ets
%% gb_trees因为结构是tuple所以就不加进去了
%% 查找结果
%% k + [{k,v}] -> v
%% {k, pos} + [tuple] -> tuple
%% k + map -> v
%% k + tuple -> element(k, tuple)
%% k + dict -> v
%% k + ets -> tuple
-export([lookup/3, store/3]).

%% {k,v}结构数学运算, 从左到右
%% 支持 [{k,v}], map, dict, ets, 结构不能嵌套
%% [s1,s2,s3,s4], 执行顺序, s1 + (遍历s2 => 遍历s3 => 遍历s4)
%% 最终产出的是s1的结构
%% 若key不存在, 默认值为0
-export([plus/1, subtract/1, multiply/1, divide/1]).

%% 对(k,v)执行一个函数,
%% 对于, lookup+store, 可以减少一次store时的lookup
%% 大概对应lookup和store的逻辑
-export([apply_if_exist/4, apply/4]).

%% 从dict.erl复制过来, erl版本变化时可能会有问题, ?PTOLEMAIOS_MASK_COPY_FROM_ERL
-record(dict, {size, n, maxn, bso, exp_size, con_size, empty, segs}).
%% 如果使用list作为key, 外面必须再套一层[]
%% 特别支持key+pos, 非{k,v}元组
-type key() :: integer()|{Key :: term(), Pos :: non_neg_integer()}|term().
-type value() :: term().
%% ets仅支持set和ordered_set, 不要(正常也不会)定义dict的record
-type struct() :: tuple()| [{key(), value()}|tuple()]|map()|dict:dict()|ets:tab().

%% 默认值
-type default() ::
%% 支持apply/2,3, 所以不可以存储以下元组
?KV_OP_DEF(M, F)|?KV_OP_DEF(M, F, A)
|?KV_OP_DEF(F)|?KV_OP_DEF(F, A)
|?KV_OP_DEF% '_'代表该值必定存在, 不存在的话会报错
|term().% 默认值

%% 数学运算
%% (k,v), 仅支持v是数字, 不支持嵌套(Struct(K,Struct(K,V))), 嵌套的key构造不了
-type math_kv() :: term().
-type math_list() :: [math_kv()].


%% @doc 查找一个值
-spec lookup(key()|[key()], struct(), value()) -> error|{ok, value()}.
lookup([], Struct, _Default) ->
    {ok, Struct};
lookup([{Key, Pos} | T], Struct, Default) ->%% 这个是特别的
    case lists:keyfind(Key, Pos, Struct) of
        false -> Default;
        Tuple -> lookup(T, Tuple, Default)
    end;
lookup([H | T], Struct, Default) when is_list(Struct) ->
    case lists:keyfind(H, 1, Struct) of
        false -> Default;
        {_, Value} -> lookup(T, Value, Default)
    end;
lookup([H | T], Struct, Default) when is_record(Struct, dict) ->
    case dict:find(H, Struct) of
        error -> Default;
        {ok, Value} -> lookup(T, Value, Default)
    end;
lookup([H | T], Struct, Default) when is_tuple(Struct) ->
    Value = element(H, Struct),
    lookup(T, Value, Default);
lookup([H | T], Struct, Default) when is_map(Struct) ->
    case Struct of
        #{H := Value} -> lookup(T, Value, Default);
        _ -> Default
    end;
lookup([H | T], Struct, Default) when is_atom(Struct);is_reference(Struct) ->
    case ets:lookup(Struct, H) of
        [Value] -> lookup(T, Value, Default);
        _ -> Default
    end;
lookup(Key, Struct, Default) ->
    lookup([Key], Struct, Default).

%% @doc 存储一个值
%% 注意最后一个default值是无效果的, 因为value值会直接覆盖
%% 所以当直接传入key时可以不写default ^ ^
-spec store(key()|[{key(), default()}], value(), struct()) -> struct().
store([{{Key, Pos} = SKey, Default} | T], Value, Struct) ->
    case T of
        [] ->
            lists:keystore(Key, Pos, Struct, Value);
        _ ->
            case lists:keyfind(Key, Pos, Struct) of
                false -> SValue = make_default(Default, SKey);
                SValue -> ok
            end,
            SValue1 = store(T, Value, SValue),
            lists:keystore(Key, Pos, Struct, SValue1)
    end;
store([{Key, Default} | T], Value, Struct) when is_list(Struct) ->
    case T of
        [] ->
            lists:keystore(Key, 1, Struct, {Key, Value});
        _ ->
            case lists:keyfind(Key, 1, Struct) of
                false -> SValue = make_default(Default, Key);
                {_, SValue} -> ok
            end,
            SValue1 = store(T, Value, SValue),
            lists:keystore(Key, 1, Struct, {Key, SValue1})
    end;
store([{Key, Default} | T], Value, Struct) when is_record(Struct, dict) ->
    case T of
        [] ->
            dict:store(Key, Value, Struct);
        _ ->
            case dict:find(Key, Struct) of
                error -> SValue = make_default(Default, Key);
                {_, SValue} -> ok
            end,
            SValue1 = store(T, Value, SValue),
            dict:store(Key, SValue1, Struct)
    end;
store([{Key, _Default} | T], Value, Struct) when is_tuple(Struct) ->
    case T of
        [] ->
            setelement(Key, Struct, Value);
        _ ->
            SValue = element(Key, Struct),
            SValue1 = store(T, Value, SValue),
            setelement(Key, Struct, SValue1)
    end;
store([{Key, Default} | T], Value, Struct) when is_map(Struct) ->
    case T of
        [] ->
            Struct#{Key => Value};
        _ ->
            case Struct of
                #{Key := SValue} -> ok;
                _ -> SValue = make_default(Default, Key)
            end,
            SValue1 = store(T, Value, SValue),
            Struct#{Key => SValue1}
    end;
store([{Key, Default} | T], Value, Struct) when is_atom(Struct);is_reference(Struct) ->
    case T of
        [] ->
            ets:insert(Struct, Value);
        _ ->
            case ets:lookup(Struct, Key) of
                [SValue] -> ok;
                _ -> SValue = make_default(Default, Key)
            end,
            SValue1 = store(T, Value, SValue),
            ets:insert(Struct, SValue1)
    end,
    %% ets 不会变
    Struct;
store(Key, Value, Struct) ->
    store([{Key, undefined}], Value, Struct).

make_default(?KV_OP_DEF(M, F), _Key) when is_atom(M), is_atom(F) ->
    apply(M, F, []);
make_default(?KV_OP_DEF(M, F, A), _Key) ->
    apply(M, F, A);
make_default(?KV_OP_DEF(F), _Key) when is_function(F) ->
    apply(F, []);
make_default(?KV_OP_DEF(F, A), _Key) when is_function(F) ->
    apply(F, A);
make_default(?KV_OP_DEF, Key) ->
    throw({noexist, Key});
make_default(Term, _Key) ->
    Term.

%% @doc 加法
-spec plus(math_list()) -> math_kv().
plus(List) ->
    math(plus, List).

%% @doc 减法
-spec subtract(math_list()) -> math_kv().
subtract(List) ->
    math(subtract, List).

%% @doc 乘法
-spec multiply(math_list()) -> math_kv().
multiply(List) ->
    math(multiply, List).

%% @doc 除法
-spec divide(math_list()) -> math_kv().
divide(List) ->
    math(divide, List).

math(_Op, []) ->
    [];
math(_Op, [H]) ->
    H;
math(Op, [H1, H2 | T]) when is_list(H2) ->
    H =
        lists:foldl(fun({Key, Value}, Acc) ->
            math_h1(Op, Key, Value, Acc)
                    end, H1, H2),
    math(Op, [H | T]);
math(Op, [H1, H2 | T]) when is_map(H2) ->
    H =
        maps:fold(fun(Key, Value, Acc) ->
            math_h1(Op, Key, Value, Acc)
                  end, H1, H2),
    math(Op, [H | T]);
math(Op, [H1, H2 | T]) when is_record(H2, dict) ->
    H =
        dict:fold(fun(Key, Value, Acc) ->
            math_h1(Op, Key, Value, Acc)
                  end, H1, H2),
    math(Op, [H | T]);
math(Op, [H1, H2 | T]) when is_atom(H2);is_reference(H2) ->
    H =
        ets:foldl(fun({Key, Value}, Acc) ->
            math_h1(Op, Key, Value, Acc)
                  end, H1, H2),
    math(Op, [H | T]).

math_h1(Op, Key, Value, H1) when is_list(H1) ->
    case lists:keyfind(Key, 1, H1) of
        false -> OldValue = 0;
        {_, OldValue} -> ok
    end,
    lists:keystore(Key, 1, H1, {Key, math_op(Op, OldValue, Value)});
math_h1(Op, Key, Value, H1) when is_map(H1) ->
    case maps:get(Key, H1, false) of
        false -> OldValue = 0;
        OldValue -> ok
    end,
    H1#{Key => math_op(Op, OldValue, Value)};
math_h1(Op, Key, Value, H1) when is_record(H1, dict) ->
    case dict:find(Key, H1) of
        error -> OldValue = 0;
        {ok, OldValue} -> ok
    end,
    dict:store(Key, math_op(Op, OldValue, Value), H1);
math_h1(Op, Key, Value, H1) when is_atom(H1);is_reference(H1) ->
    case ets:lookup(H1, Key) of
        [{_, OldValue}] -> ok;
        _ -> OldValue = 0
    end,
    ets:insert(H1, {Key, math_op(Op, OldValue, Value)}),
    H1.

%% 这里可以扩展
math_op(plus, V1, V2) -> V1 + V2;
math_op(subtract, V1, V2) -> V1 - V2;
math_op(multiply, V1, V2) -> V1 * V2;
math_op(divide, V1, V2) -> V1 / V2.

%% @doc 执行一个函数如果值存在
%% 若函数返回DeleteMask, 则删除该值
%% 若函数执行, 返回Struct, 否则返回error
%% 所以ets不能使用表名'error'
-spec apply_if_exist(fun((K :: key(), V :: value()) -> V1 :: value()),
    key()|[key()], struct(), term()) -> error| struct().
apply_if_exist(_F, [], _Struct, _DeleteMask) ->
    error;
apply_if_exist(F, [{Key, Pos} | T], Struct, DeleteMask) ->%% 这个是特别的
    case lists:keyfind(Key, Pos, Struct) of
        false -> error;
        Tuple ->
            case T of
                [] ->
                    case F(Key, Tuple) of
                        DeleteMask -> lists:keydelete(Key, Pos, Struct);
                        Tuple1 -> lists:keystore(Key, Pos, Struct, Tuple1)
                    end;
                _ ->
                    case apply_if_exist(F, T, Tuple, DeleteMask) of
                        error -> error;
                        Tuple1 -> lists:keystore(Key, Pos, Struct, Tuple1)
                    end
            end
    end;
apply_if_exist(F, [H | T], Struct, DeleteMask) when is_list(Struct) ->
    case lists:keyfind(H, 1, Struct) of
        false -> error;
        {_, Value} ->
            case T of
                [] ->
                    case F(H, Value) of
                        DeleteMask -> lists:keydelete(H, 1, Struct);
                        Value1 -> lists:keystore(H, 1, Struct, {H, Value1})
                    end;
                _ ->
                    case apply_if_exist(F, T, Value, DeleteMask) of
                        error -> error;
                        Value1 -> lists:keystore(H, 1, Struct, {H, Value1})
                    end
            end
    end;
apply_if_exist(F, [H | T], Struct, DeleteMask) when is_record(Struct, dict) ->
    case dict:find(H, Struct) of
        error -> error;
        {ok, Value} ->
            case T of
                [] ->
                    case F(H, Value) of
                        DeleteMask -> dict:erase(H, Struct);
                        Value1 -> dict:store(H, Value1, Struct)
                    end;
                _ ->
                    case apply_if_exist(F, T, Value, DeleteMask) of
                        error -> error;
                        Value1 -> dict:store(H, Value1, Struct)
                    end
            end
    end;
apply_if_exist(F, [H | T], Struct, DeleteMask) when is_tuple(Struct) ->
    Value = element(H, Struct),
    case T of
        [] ->
            case F(H, Value) of
                %% 显然这个参数不应该生效
                DeleteMask -> throw(badarg);
                Value1 -> setelement(H, Struct, Value1)
            end;
        _ ->
            case apply_if_exist(F, T, Value, DeleteMask) of
                error -> error;
                Value1 -> setelement(H, Struct, Value1)
            end
    end;
apply_if_exist(F, [H | T], Struct, DeleteMask) when is_map(Struct) ->
    case Struct of
        #{H := Value} ->
            case T of
                [] ->
                    case F(H, Value) of
                        DeleteMask -> maps:remove(H, Struct);
                        Value1 -> Struct#{H => Value1}
                    end;
                _ ->
                    case apply_if_exist(F, T, Value, DeleteMask) of
                        error -> error;
                        Value1 -> Struct#{H => Value1}
                    end
            end;
        _ -> error
    end;
apply_if_exist(F, [H | T], Struct, DeleteMask) when is_atom(Struct);is_reference(Struct) ->
    case ets:lookup(Struct, H) of
        [Value] ->
            case T of
                [] ->
                    case F(H, Value) of
                        DeleteMask -> ets:delete(Struct, H);
                        Value1 -> ets:insert(Struct, Value1)
                    end,
                    Struct;
                _ ->
                    case apply_if_exist(F, T, Value, DeleteMask) of
                        error -> error;
                        Value1 ->
                            ets:insert(Struct, Value1),
                            Struct
                    end
            end;
        _ -> error
    end;
apply_if_exist(F, Key, Struct, DeleteMask) ->
    apply_if_exist(F, [Key], Struct, DeleteMask).

%% @doc 执行一个函数, 如果值不存在则使用默认值
%% 若函数返回DeleteMask, 则删除该值
-spec apply(fun((K :: key(), V :: value()) -> V1 :: value()),
    {key(), default()}|[{key(), default()}], struct(), term()) -> struct().
apply(F, [{{Key, Pos}, Default} | T], Struct, DeleteMask) ->%% 这个是特别的
    case lists:keyfind(Key, Pos, Struct) of
        false -> Tuple = make_default(Default, Key);
        Tuple -> ok
    end,
    case T of
        [] ->
            case F(Key, Tuple) of
                DeleteMask -> lists:keydelete(Key, Pos, Struct);
                Tuple1 -> lists:keystore(Key, Pos, Struct, Tuple1)
            end;
        _ ->
            case apply(F, T, Tuple, DeleteMask) of
                error -> error;
                Tuple1 -> lists:keystore(Key, Pos, Struct, Tuple1)
            end
    end;
apply(F, [{H, Default} | T], Struct, DeleteMask) when is_list(Struct) ->
    case lists:keyfind(H, 1, Struct) of
        false -> Value = make_default(Default, H);
        {_, Value} -> ok
    end,
    case T of
        [] ->
            case F(H, Value) of
                DeleteMask -> lists:keydelete(H, 1, Struct);
                Value1 -> lists:keystore(H, 1, Struct, {H, Value1})
            end;
        _ ->
            case apply(F, T, Value, DeleteMask) of
                error -> error;
                Value1 -> lists:keystore(H, 1, Struct, {H, Value1})
            end
    end;
apply(F, [{H, Default} | T], Struct, DeleteMask) when is_record(Struct, dict) ->
    case dict:find(H, Struct) of
        error -> Value = make_default(Default, H);
        {ok, Value} -> ok
    end,
    case T of
        [] ->
            case F(H, Value) of
                DeleteMask -> dict:erase(H, Struct);
                Value1 -> dict:store(H, Value1, Struct)
            end;
        _ ->
            case apply(F, T, Value, DeleteMask) of
                error -> error;
                Value1 -> dict:store(H, Value1, Struct)
            end
    end;
apply(F, [{H, _Default} | T], Struct, DeleteMask) when is_tuple(Struct) ->
    Value = element(H, Struct),
    case T of
        [] ->
            case F(H, Value) of
                %% 显然这个参数不应该生效
                DeleteMask -> throw(badarg);
                Value1 -> setelement(H, Struct, Value1)
            end;
        _ ->
            case apply(F, T, Value, DeleteMask) of
                error -> error;
                Value1 -> setelement(H, Struct, Value1)
            end
    end;
apply(F, [{H, Default} | T], Struct, DeleteMask) when is_map(Struct) ->
    case Struct of
        #{H := Value} -> ok;
        _ -> Value = make_default(Default, H)
    end,
    case T of
        [] ->
            case F(H, Value) of
                DeleteMask -> maps:remove(H, Struct);
                Value1 -> Struct#{H => Value1}
            end;
        _ ->
            case apply(F, T, Value, DeleteMask) of
                error -> error;
                Value1 -> Struct#{H => Value1}
            end
    end;
apply(F, [{H, Default} | T], Struct, DeleteMask) when is_atom(Struct);is_reference(Struct) ->
    case ets:lookup(Struct, H) of
        [Value] -> ok;
        _ -> Value = make_default(Default, H)
    end,
    case T of
        [] ->
            case F(H, Value) of
                DeleteMask -> ets:delete(Struct, H);
                Value1 -> ets:insert(Struct, Value1)
            end,
            Struct;
        _ ->
            case apply(F, T, Value, DeleteMask) of
                error -> error;
                Value1 ->
                    ets:insert(Struct, Value1),
                    Struct
            end
    end;
apply(F, Key, Struct, DeleteMask) ->
    apply(F, [Key], Struct, DeleteMask).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

base_test_() ->
    Struct = [{list, [{'_', tuple_list, {'_', #{map => dict:store(dict, ?MODULE, dict:new())}}}]}],
    [
        {setup,
            fun() ->
                ets:new(?MODULE, [named_table, public]),
                ets:insert(?MODULE, {ets, ok})
            end,
            fun(_) ->
                ets:delete(?MODULE)
            end,
            fun(_) ->
                %% list, tuple_list, tuple, map, dict, ets
                [
                    ?_assertEqual({ok, {ets, ok}},
                        kv_op:lookup([list, {tuple_list, 2}, 3, 2, map, dict, ets], Struct, undefined)),
                    ?_assertEqual(undefined,
                        kv_op:lookup([list, {tuple_list, 2}, 3, 2, map, dict, error], Struct, undefined)),
                    ?_assertError(badarg,
                        kv_op:lookup([list, {tuple_list, 2}, 3, 2, map, dict, ets, error], Struct, undefined)),
                    ?_assertEqual(Struct,
                        kv_op:store([
                            {list, []},
                            {{tuple_list, 2}, {'_', tuple_list, {'_', #{}}}},
                            {3, '_'},
                            {2, '_'},
                            {map, ?KV_OP_DEF(dict, new)},
                            {dict, ?MODULE},
                            {lookup, {lookup, default}}
                        ], {lookup, ok}, [])),
                    ?_assertEqual([{lookup, ok}], ets:lookup(?MODULE, lookup)),
                    ?_assertEqual([{1, 1}], kv_op:store(1, 1, [])),
                    
                    ?_test(ets:delete_all_objects(?MODULE)),
                    ?_test(ets:insert(?MODULE, [{1, 1}, {2, 1}, {3, 1}])),
                    ?_assertError(badarith, kv_op:plus([[], #{1 => a}])),
                    ?_assertEqual([{1, 3}, {2, 2}, {3, 1}], kv_op:plus([
                        [], #{1 => 1},
                        dict:store(1, 1, dict:store(2, 1, dict:new())), ?MODULE])),
                    ?_assertEqual([{1, 0}, {2, 0}, {3, 0}], kv_op:subtract([
                        [{1, 3}, {2, 2}, {3, 1}], #{1 => 1},
                        dict:store(1, 1, dict:store(2, 1, dict:new())), ?MODULE])),
                    ?_assertEqual([{1, 1}, {2, 4}, {3, 3}], kv_op:multiply([
                        [{1, 1}, {2, 2}, {3, 3}], #{1 => 1},
                        dict:store(1, 1, dict:store(2, 2, dict:new())), ?MODULE])),
                    ?_assertEqual([{1, 1.0}, {2, 1.0}, {3, 3.0}], kv_op:divide([
                        [{1, 1}, {2, 2}, {3, 3}], #{1 => 1},
                        dict:store(1, 1, dict:store(2, 2, dict:new())), ?MODULE])),
                    
                    ?_test(ets:delete_all_objects(?MODULE)),
                    ?_test(ets:insert(?MODULE, {ets, ok})),
                    ?_assertEqual(Struct,
                        kv_op:apply_if_exist(fun(_, _) ->
                            undefined
                                             end, [list, {tuple_list, 2}, 3, 2, map, dict, ets], Struct, undefined)),
                    ?_assert(ets:insert_new(?MODULE, {ets, ok})),
                    ?_assertEqual(Struct,
                        kv_op:apply_if_exist(fun(_, _) ->
                            {ets, replace}
                                             end, [list, {tuple_list, 2}, 3, 2, map, dict, ets], Struct, undefined)),
                    ?_assertEqual([{ets, replace}], ets:lookup(?MODULE, ets)),
                    ?_assertEqual(error,
                        kv_op:apply_if_exist(fun(_, _) ->
                            undefined
                                             end, [list, {tuple_list, 2}, 3, 2, map, dict, error], Struct, undefined)),
                    ?_assertError(badarg,
                        kv_op:apply_if_exist(fun(_, _) ->
                            undefined
                                             end, [list, {tuple_list, 2}, 3, 2, map, dict, ets, error], Struct, undefined)),
                    ?_assertEqual(Struct,
                        kv_op:apply(fun(lookup, {lookup, default}) ->
                            {lookup, ok}
                                    end,
                            [
                                {list, []},
                                {{tuple_list, 2}, {'_', tuple_list, {'_', #{}}}},
                                {3, '_'},
                                {2, '_'},
                                {map, ?KV_OP_DEF(dict, new)},
                                {dict, ?MODULE},
                                {lookup, {lookup, default}}
                            ], [], undefined)),
                    ?_assertEqual([{lookup, ok}], ets:lookup(?MODULE, lookup))
                ]
            end}
    ].

-endif.