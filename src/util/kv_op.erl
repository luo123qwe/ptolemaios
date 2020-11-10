%%%-------------------------------------------------------------------
%%% @author dominic
%%% @doc 键值型数据结构操作
%%%
%%% <b>第一部分, 对于以下深嵌套的结构kv场景</b>
%%%
%%% ```
%%% {_, List2} = lists:keyfind(Key1, Pos1, List1),
%%% {_, Dict3} = lists:keyfind(Key2, Pos2, List2)
%%% {ok, #record{x = X}} = dict:find(Key3, Dict3)'''
%%%
%%% 查询和更改都要一层层find和store, 本模块尝试把这个流程合并成一个key列表, 查询和存储都可以一句代码完成
%%%
%%% 存在一点点效率损耗, 判断数据类型和存储的时候会再次查找一遍
%%%
%%% 支持 tuple_list, map, tuple, map, dict, ets, key+struct对应结果:
%%% ```
%%% k + [{k,v}] -> v
%%% {k, pos} + [tuple] -> tuple
%%% k + map -> v
%%% k + tuple -> element(k, tuple)
%%% k + dict -> v
%%% k + ets -> tuple'''
%%%
%%% <b>第二部分, 提供加减乘除数学运算</b>
%%%
%%% 从左到右, 支持 [{k,v}], map, dict, ets, 且结构不能嵌套
%%%
%%% [s1,s2,s3,s4], 执行顺序, s1 + (遍历s2 => 遍历s3 => 遍历s4)
%%%
%%% 最终产出的是s1的结构, 若key不存在, 默认值为0
%%%
%%% <b>第三部分, 直接在内部执行一个函数</b>
%%%
%%% 对第一部分进一步的优化, 可以减少一次store时的lookup
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
-export([lookup/3, store/3, delete/2, store_with_default/3]).

%% {k,v}结构数学运算, 从左到右
%% 支持 [{k,v}], map, dict, ets, 结构不能嵌套
%% [s1,s2,s3,s4], 执行顺序, s1 + (遍历s2 => 遍历s3 => 遍历s4)
%% 最终产出的是s1的结构
%% 若key不存在, 默认值为0
-export([plus/1, subtract/1, multiply/1, divide/1]).

%% 对(k,v)执行一个函数,
%% 对于, lookup+store, 可以减少一次store时的lookup
%% 大概对应lookup和store的逻辑
-export([update/3, update_with_default/3]).

%% 从dict.erl复制过来, erl版本变化时可能会有问题, ?PTOLEMAIOS_MASK_COPY_FROM_ERL
-record(dict, {size, n, maxn, bso, exp_size, con_size, empty, segs}).

%% 如果使用list作为key, 外面必须再套一层[]
%% 支持key+pos, 非{k,v}元组, 对应lists+record的操作
%% 支持ets+pos, 对应ets:lookup/update_element, store时ets的值不存在则使用default插入一条新数据
-type key() ::
integer()|
ets:tab()|
{Key :: term(), Pos :: non_neg_integer()}|
{ets:tab(), Pos :: integer()}|
term().

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
-spec lookup(key()|[key()], struct(), value()) -> error|value().
lookup([], Struct, _Default) ->
    Struct;
lookup([{Key, Pos} | T], Struct, Default) when is_list(Struct) ->%% 这个是特别的
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
lookup([{Key, Pos} | T], Struct, Default) when is_atom(Struct);is_reference(Struct) ->
    Value =
        try ets:lookup_element(Struct, Key, Pos)
        catch
            _:_ -> undefined
        end,
    case Value of
        undefined -> Default;
        _ -> lookup(T, Value, Default)
    end;
lookup([H | T], Struct, Default) when is_atom(Struct);is_reference(Struct) ->
    case ets:lookup(Struct, H) of
        [Value] -> lookup(T, Value, Default);
        _ -> Default
    end;
lookup(Key, Struct, Default) ->
    lookup([Key], Struct, Default).

%% @doc 存储一个值
-spec store(key()|[key()], value(), struct()) -> struct().
store([{Key, Pos} | T], Value, Struct) when is_list(Struct) ->
    case T of
        [] ->
            lists:keystore(Key, Pos, Struct, Value);
        _ ->
            case lists:keyfind(Key, Pos, Struct) of
                false ->
                    throw(badarg);
                SValue ->
                    SValue1 = store(T, Value, SValue),
                    lists:keystore(Key, Pos, Struct, SValue1)
            end
    end;
store([Key | T], Value, Struct) when is_list(Struct) ->
    case T of
        [] ->
            lists:keystore(Key, 1, Struct, {Key, Value});
        _ ->
            case lists:keyfind(Key, 1, Struct) of
                false ->
                    throw(badarg);
                {_, SValue} ->
                    SValue1 = store(T, Value, SValue),
                    lists:keystore(Key, 1, Struct, {Key, SValue1})
            end
    end;
store([Key | T], Value, Struct) when is_record(Struct, dict) ->
    case T of
        [] ->
            dict:store(Key, Value, Struct);
        _ ->
            case dict:find(Key, Struct) of
                error ->
                    throw(badarg);
                {_, SValue} ->
                    SValue1 = store(T, Value, SValue),
                    dict:store(Key, SValue1, Struct)
            end
    end;
store([Key | T], Value, Struct) when is_tuple(Struct) ->
    case T of
        [] ->
            setelement(Key, Struct, Value);
        _ ->
            SValue = element(Key, Struct),
            SValue1 = store(T, Value, SValue),
            setelement(Key, Struct, SValue1)
    end;
store([Key | T], Value, Struct) when is_map(Struct) ->
    case T of
        [] ->
            Struct#{Key => Value};
        _ ->
            case Struct of
                #{Key := SValue} ->
                    SValue1 = store(T, Value, SValue),
                    Struct#{Key => SValue1};
                _ ->
                    throw(badarg)
            end
    end;
store([{Key, Pos} | T], Value, Struct) when is_atom(Struct);is_reference(Struct) ->
    case T of
        [] ->
            ets:update_element(Struct, Key, {Pos, Value});
        _ ->
            SValue1 = ets:lookup_element(Struct, Key, Pos),
            SValue2 = store(T, Value, SValue1),
            ets:update_element(Struct, Key, {Pos, SValue2})
    end,
    %% ets 不会变
    Struct;
store([Key | T], Value, Struct) when is_atom(Struct);is_reference(Struct) ->
    case T of
        [] ->
            ets:insert(Struct, Value);
        _ ->
            case ets:lookup(Struct, Key) of
                [SValue] ->
                    SValue1 = store(T, Value, SValue),
                    ets:insert(Struct, SValue1);
                _ ->
                    throw(badarg)
            end
    end,
    %% ets 不会变
    Struct;
store(Key, Value, Struct) ->
    store([{Key, undefined}], Value, Struct).

%% @doc 存储一个值, 附带默认值{key, default}
%%
%% 注意最后一个default值是无效果的, 因为value值会直接覆盖
%%
%% 所以当直接传入key时可以不写default ^ ^
-spec store_with_default(key()|[{key(), default()}], value(), struct()) -> struct().
store_with_default([{{Key, Pos} = SKey, Default} | T], Value, Struct) when is_list(Struct) ->
    case T of
        [] ->
            lists:keystore(Key, Pos, Struct, Value);
        _ ->
            case lists:keyfind(Key, Pos, Struct) of
                false -> SValue = make_default(Default, SKey);
                SValue -> ok
            end,
            SValue1 = store_with_default(T, Value, SValue),
            lists:keystore(Key, Pos, Struct, SValue1)
    end;
store_with_default([{Key, Default} | T], Value, Struct) when is_list(Struct) ->
    case T of
        [] ->
            lists:keystore(Key, 1, Struct, {Key, Value});
        _ ->
            case lists:keyfind(Key, 1, Struct) of
                false -> SValue = make_default(Default, Key);
                {_, SValue} -> ok
            end,
            SValue1 = store_with_default(T, Value, SValue),
            lists:keystore(Key, 1, Struct, {Key, SValue1})
    end;
store_with_default([{Key, Default} | T], Value, Struct) when is_record(Struct, dict) ->
    case T of
        [] ->
            dict:store(Key, Value, Struct);
        _ ->
            case dict:find(Key, Struct) of
                error -> SValue = make_default(Default, Key);
                {_, SValue} -> ok
            end,
            SValue1 = store_with_default(T, Value, SValue),
            dict:store(Key, SValue1, Struct)
    end;
store_with_default([{Key, _Default} | T], Value, Struct) when is_tuple(Struct) ->
    case T of
        [] ->
            setelement(Key, Struct, Value);
        _ ->
            SValue = element(Key, Struct),
            SValue1 = store_with_default(T, Value, SValue),
            setelement(Key, Struct, SValue1)
    end;
store_with_default([{Key, Default} | T], Value, Struct) when is_map(Struct) ->
    case T of
        [] ->
            Struct#{Key => Value};
        _ ->
            case Struct of
                #{Key := SValue} -> ok;
                _ -> SValue = make_default(Default, Key)
            end,
            SValue1 = store_with_default(T, Value, SValue),
            Struct#{Key => SValue1}
    end;
store_with_default([{{Key, Pos}, Default} | T], Value, Struct) when is_atom(Struct);is_reference(Struct) ->
    case T of
        [] ->
            try ets:update_element(Struct, Key, {Pos, Value})
            catch
                _:_ ->
                    ets:insert(Struct, Default)
            end;
        _ ->
            SValue1 =
                try ets:lookup_element(Struct, Key, Pos)
                catch
                    _:_ ->
                        ets:insert(Struct, Default),
                        element(Pos, Default)
                end,
            SValue2 = store_with_default(T, Value, SValue1),
            ets:update_element(Struct, Key, {Pos, SValue2})
    end,
    %% ets 不会变
    Struct;
store_with_default([{Key, Default} | T], Value, Struct) when is_atom(Struct);is_reference(Struct) ->
    case T of
        [] ->
            ets:insert(Struct, Value);
        _ ->
            case ets:lookup(Struct, Key) of
                [SValue] -> ok;
                _ -> SValue = make_default(Default, Key)
            end,
            SValue1 = store_with_default(T, Value, SValue),
            ets:insert(Struct, SValue1)
    end,
    %% ets 不会变
    Struct;
store_with_default(Key, Value, Struct) ->
    store_with_default([{Key, undefined}], Value, Struct).

%% @doc 删除一个值
-spec delete(key()|[key()], struct()) -> struct().
delete([{Key, Pos} | T], Struct) when is_list(Struct) ->
    case T of
        [] ->
            lists:keydelete(Key, Pos, Struct);
        _ ->
            case lists:keyfind(Key, Pos, Struct) of
                false ->
                    Struct;
                SValue ->
                    SValue1 = delete(T, SValue),
                    lists:keystore(Key, Pos, Struct, SValue1)
            end
    end;
delete([Key | T], Struct) when is_list(Struct) ->
    case T of
        [] ->
            lists:keydelete(Key, 1, Struct);
        _ ->
            case lists:keyfind(Key, 1, Struct) of
                false ->
                    Struct;
                {_, SValue} ->
                    SValue1 = delete(T, SValue),
                    lists:keystore(Key, 1, Struct, {Key, SValue1})
            end
    end;
delete([Key | T], Struct) when is_record(Struct, dict) ->
    case T of
        [] ->
            dict:erase(Key, Struct);
        _ ->
            case dict:find(Key, Struct) of
                error ->
                    Struct;
                {_, SValue} ->
                    SValue1 = delete(T, SValue),
                    dict:store(Key, SValue1, Struct)
            end
    end;
delete([Key | T], Struct) when is_map(Struct) ->
    case T of
        [] ->
            maps:remove(Key, Struct);
        _ ->
            case Struct of
                #{Key := SValue} ->
                    SValue1 = delete(T, SValue),
                    Struct#{Key => SValue1};
                _ ->
                    Struct
            end
    end;
delete([{Key, Pos} | T], Struct) when is_atom(Struct);is_reference(Struct) ->
    case T of
        [] ->
            %% 元组没有delete操作
            throw(badarg);
        _ ->
            SValue =
                try ets:lookup_element(Struct, Key, Pos)
                catch _:_ ->
                    undefined
                end,
            %% 保证其他代码报错直接报错
            case SValue of
                undefined ->
                    skip;
                _ ->
                    SValue1 = delete(T, SValue),
                    ets:insert(Struct, SValue1)
            end
    end,
    %% ets 不会变
    Struct;
delete([Key | T], Struct) when is_atom(Struct);is_reference(Struct) ->
    case T of
        [] ->
            ets:delete(Struct, Key);
        _ ->
            case ets:lookup(Struct, Key) of
                [SValue] ->
                    SValue1 = delete(T, SValue),
                    ets:insert(Struct, SValue1);
                _ ->
                    ok
            end
    end,
    %% ets 不会变
    Struct;
delete(Key, Struct) ->
    delete([Key], Struct).

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

%% @doc 执行一个函数如果值存在, 少一次lookup, 但是需要apply
%%
%% 若函数返回{true, Return}, 则删除该值并放回Return
%%
%% 若函数返回{false, Return, NewValue}, 则更新值并放回Return
%%
%% 若数据全部存在, 函数执行, 否则返回undefined
%%
%% 所以ets不能使用表名'undefined'
-spec update(fun((K :: key(), V :: value()) -> {true, Return :: term()}|{false, Return :: term(), V1 :: value()}),
    key()|[key()], struct()) -> undefined| {Return :: term(), struct()}.
update(_F, [], _Struct) ->
    undefined;
update(F, [{Key, Pos} | T], Struct) when is_list(Struct) ->%% 这个是特别的
    case lists:keyfind(Key, Pos, Struct) of
        false -> undefined;
        Tuple ->
            case T of
                [] ->
                    case F(Key, Tuple) of
                        {true, Return} ->
                            {Return, lists:keydelete(Key, Pos, Struct)};
                        {_, Return, Tuple1} ->
                            {Return, lists:keystore(Key, Pos, Struct, Tuple1)}
                    end;
                _ ->
                    case update(F, T, Tuple) of
                        {Return, Tuple1} ->
                            {Return, lists:keystore(Key, Pos, Struct, Tuple1)};
                        _ ->
                            undefined
                    end
            end
    end;
update(F, [H | T], Struct) when is_list(Struct) ->
    case lists:keyfind(H, 1, Struct) of
        false -> undefined;
        {_, Value} ->
            case T of
                [] ->
                    case F(H, Value) of
                        {true, Return} ->
                            {Return, lists:keydelete(H, 1, Struct)};
                        {_, Return, Value1} ->
                            {Return, lists:keystore(H, 1, Struct, {H, Value1})}
                    end;
                _ ->
                    case update(F, T, Value) of
                        {Return, Value1} ->
                            {Return, lists:keystore(H, 1, Struct, {H, Value1})};
                        _ ->
                            undefined
                    end
            end
    end;
update(F, [H | T], Struct) when is_record(Struct, dict) ->
    case dict:find(H, Struct) of
        error -> undefined;
        {ok, Value} ->
            case T of
                [] ->
                    case F(H, Value) of
                        {true, Return} ->
                            {Return, dict:erase(H, Struct)};
                        {_, Return, Value1} ->
                            {Return, dict:store(H, Value1, Struct)}
                    end;
                _ ->
                    case update(F, T, Value) of
                        {Return, Value1} ->
                            {Return, dict:store(H, Value1, Struct)};
                        _ ->
                            undefined
                    end
            end
    end;
update(F, [H | T], Struct) when is_tuple(Struct) ->
    Value = element(H, Struct),
    case T of
        [] ->
            case F(H, Value) of
                %% 显然这个参数不应该生效
                {true, _} ->
                    throw(badarg);
                {_, Return, Value1} ->
                    {Return, setelement(H, Struct, Value1)}
            end;
        _ ->
            case update(F, T, Value) of
                {Return, Value1} ->
                    {Return, setelement(H, Struct, Value1)};
                _ ->
                    undefined
            end
    end;
update(F, [H | T], Struct) when is_map(Struct) ->
    case Struct of
        #{H := Value} ->
            case T of
                [] ->
                    case F(H, Value) of
                        {true, Return} ->
                            {Return, maps:remove(H, Struct)};
                        {_, Return, Value1} ->
                            {Return, Struct#{H => Value1}}
                    end;
                _ ->
                    case update(F, T, Value) of
                        {Return, Value1} ->
                            {Return, Struct#{H => Value1}};
                        _ ->
                            undefined
                    end
            end;
        _ -> undefined
    end;
update(F, [{Key, Pos} = H | T], Struct) when is_atom(Struct);is_reference(Struct) ->
    try
        Value = ets:lookup_element(Struct, Key, Pos),
        case T of
            [] ->
                case F(H, Value) of
                    {true, Return} -> ets:delete(Struct, Key);
                    {_, Return, Value1} -> ets:update_element(Struct, Key, {Pos, Value1})
                end,
                {Return, Struct};
            _ ->
                case update(F, T, Value) of
                    {Return, Value1} ->
                        ets:update_element(Struct, Key, {Pos, Value1}),
                        {Return, Struct};
                    _ ->
                        undefined
                end
        end
    catch
        _:_ -> undefined
    end;
update(F, [H | T], Struct) when is_atom(Struct);is_reference(Struct) ->
    case ets:lookup(Struct, H) of
        [Value] ->
            case T of
                [] ->
                    case F(H, Value) of
                        {true, Return} -> ets:delete(Struct, H);
                        {_, Return, Value1} -> ets:insert(Struct, Value1)
                    end,
                    {Return, Struct};
                _ ->
                    case update(F, T, Value) of
                        {Return, Value1} ->
                            ets:insert(Struct, Value1),
                            {Return, Struct};
                        _ ->
                            undefined
                    end
            end;
        _ -> undefined
    end;
update(F, Key, Struct) ->
    update(F, [Key], Struct).

%% @doc 执行一个函数, 如果值不存在则使用默认值, 其他查看update/3
-spec update_with_default(fun((K :: key(), V :: value()) -> V1 :: value()),
    {key(), default()}|[{key(), default()}], struct()) -> struct().
update_with_default(F, [{{Key, Pos}, Default} | T], Struct) when is_list(Struct) ->%% 这个是特别的
    case lists:keyfind(Key, Pos, Struct) of
        false -> Tuple = make_default(Default, Key);
        Tuple -> ok
    end,
    case T of
        [] ->
            case F(Key, Tuple) of
                {true, Return} -> {Return, lists:keydelete(Key, Pos, Struct)};
                {_, Return, Tuple1} -> {Return, lists:keystore(Key, Pos, Struct, Tuple1)}
            end;
        _ ->
            case update_with_default(F, T, Tuple) of
                {Return, Tuple1} -> {Return, lists:keystore(Key, Pos, Struct, Tuple1)};
                _ -> undefined
            end
    end;
update_with_default(F, [{H, Default} | T], Struct) when is_list(Struct) ->
    case lists:keyfind(H, 1, Struct) of
        false -> Value = make_default(Default, H);
        {_, Value} -> ok
    end,
    case T of
        [] ->
            case F(H, Value) of
                {true, Return} -> {Return, lists:keydelete(H, 1, Struct)};
                {_, Return, Value1} -> {Return, lists:keystore(H, 1, Struct, {H, Value1})}
            end;
        _ ->
            case update_with_default(F, T, Value) of
                {Return, Value1} -> {Return, lists:keystore(H, 1, Struct, {H, Value1})};
                _ -> undefined
            end
    end;
update_with_default(F, [{H, Default} | T], Struct) when is_record(Struct, dict) ->
    case dict:find(H, Struct) of
        error -> Value = make_default(Default, H);
        {ok, Value} -> ok
    end,
    case T of
        [] ->
            case F(H, Value) of
                {true, Return} -> {Return, dict:erase(H, Struct)};
                {_, Return, Value1} -> {Return, dict:store(H, Value1, Struct)}
            end;
        _ ->
            case update_with_default(F, T, Value) of
                {Return, Value1} -> {Return, dict:store(H, Value1, Struct)};
                _ -> undefined
            end
    end;
update_with_default(F, [{H, _Default} | T], Struct) when is_tuple(Struct) ->
    Value = element(H, Struct),
    case T of
        [] ->
            case F(H, Value) of
                %% 显然这个参数不应该生效
                {true, _} -> throw(badarg);
                {_, Return, Value1} -> {Return, setelement(H, Struct, Value1)}
            end;
        _ ->
            case update_with_default(F, T, Value) of
                {Return, Value1} -> {Return, setelement(H, Struct, Value1)};
                _ -> undefined
            end
    end;
update_with_default(F, [{H, Default} | T], Struct) when is_map(Struct) ->
    case Struct of
        #{H := Value} -> ok;
        _ -> Value = make_default(Default, H)
    end,
    case T of
        [] ->
            case F(H, Value) of
                {true, Return} -> {Return, maps:remove(H, Struct)};
                {_, Return, Value1} -> {Return, Struct#{H => Value1}}
            end;
        _ ->
            case update_with_default(F, T, Value) of
                {Return, Value1} -> {Return, Struct#{H => Value1}};
                _ -> undefined
            end
    end;
update_with_default(F, [{{Key, Pos} = H, Default} | T], Struct) when is_atom(Struct);is_reference(Struct) ->
    Value =
        try ets:lookup_element(Struct, Key, Pos)
        catch
            _:_ ->
                ets:insert(Struct, Default),
                make_default(Default, H)
        end,
    case T of
        [] ->
            case F(H, Value) of
                {true, Return} -> ets:delete(Struct, Key);
                {_, Return, Value1} -> ets:update_element(Struct, Key, {Pos, Value1})
            end,
            {Return, Struct};
        _ ->
            case update_with_default(F, T, Value) of
                {Return, Value1} ->
                    ets:update_element(Struct, Key, {Pos, Value1}),
                    {Return, Struct};
                _ ->
                    undefined
            end
    end;
update_with_default(F, [{H, Default} | T], Struct) when is_atom(Struct);is_reference(Struct) ->
    case ets:lookup(Struct, H) of
        [Value] -> ok;
        _ -> Value = make_default(Default, H)
    end,
    case T of
        [] ->
            case F(H, Value) of
                {true, Return} -> ets:delete(Struct, H);
                {_, Return, Value1} -> ets:insert(Struct, Value1)
            end,
            {Return, Struct};
        _ ->
            case update_with_default(F, T, Value) of
                {Return, Value1} ->
                    ets:insert(Struct, Value1),
                    {Return, Struct};
                _ ->
                    undefined
            end
    end;
update_with_default(F, Key, Struct) ->
    update_with_default(F, [Key], Struct).

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
                    ?_assertEqual({ets, ok},
                        kv_op:lookup([list, {tuple_list, 2}, 3, 2, map, dict, ets], Struct, undefined)),
                    ?_assertEqual(undefined,
                        kv_op:lookup([list, {tuple_list, 2}, 3, 2, map, dict, error], Struct, undefined)),
                    ?_assertError(badarg,
                        kv_op:lookup([list, {tuple_list, 2}, 3, 2, map, dict, ets, error], Struct, undefined)),
                    ?_assertEqual(Struct,
                        kv_op:store_with_default([
                            {list, []},
                            {{tuple_list, 2}, {'_', tuple_list, {'_', #{}}}},
                            {3, '_'},
                            {2, '_'},
                            {map, ?KV_OP_DEF(dict, new)},
                            {dict, ?MODULE},
                            {lookup, {lookup, default}}
                        ], {lookup, ok}, [])),
                    ?_assertEqual([{lookup, ok}], ets:lookup(?MODULE, lookup)),
                    ?_assertEqual(Struct,
                        kv_op:store_with_default([
                            {list, []},
                            {{tuple_list, 2}, {'_', tuple_list, {'_', #{}}}},
                            {3, '_'},
                            {2, '_'},
                            {map, ?KV_OP_DEF(dict, new)},
                            {dict, ?MODULE},
                            {{lookup, 2}, {lookup, undefined}}
                        ], update_element, [])),
                    ?_assertEqual(update_element,
                        kv_op:lookup([list, {tuple_list, 2}, 3, 2, map, dict, {lookup, 2}], Struct, undefined)),
                    ?_assertEqual([{1, 1}], kv_op:store_with_default(1, 1, [])),
                    
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
                    ?_assertEqual({ok, Struct},
                        kv_op:update(fun(_, _) ->
                            {true, ok}
                                     end, [list, {tuple_list, 2}, 3, 2, map, dict, ets], Struct)),
                    ?_assert(ets:insert_new(?MODULE, {ets, ok})),
                    ?_assertEqual({ok, Struct},
                        kv_op:update(fun(_, _) ->
                            {false, ok, {ets, replace}}
                                     end, [list, {tuple_list, 2}, 3, 2, map, dict, ets], Struct)),
                    ?_assertEqual([{ets, replace}], ets:lookup(?MODULE, ets)),
                    ?_assertEqual(undefined,
                        kv_op:update(fun(_, _) ->
                            will_be_error
                                     end, [list, {tuple_list, 2}, 3, 2, map, dict, error], Struct)),
                    ?_assertError(badarg,
                        kv_op:update(fun(_, _) ->
                            will_be_error
                                     end, [list, {tuple_list, 2}, 3, 2, map, dict, ets, error], Struct)),
                    ?_assertEqual({ok, Struct},
                        kv_op:update_with_default(fun(lookup, {lookup, default}) ->
                            {false, ok, {lookup, ok}}
                                                  end,
                            [
                                {list, []},
                                {{tuple_list, 2}, {'_', tuple_list, {'_', #{}}}},
                                {3, '_'},
                                {2, '_'},
                                {map, ?KV_OP_DEF(dict, new)},
                                {dict, ?MODULE},
                                {lookup, {lookup, default}}
                            ], [])),
                    ?_assertEqual([{lookup, ok}], ets:lookup(?MODULE, lookup)),
                    ?_assertEqual({ok, Struct},
                        kv_op:update_with_default(fun({lookup, 2}, ok) ->
                            {false, ok, update_element}
                                                  end,
                            [
                                {list, []},
                                {{tuple_list, 2}, {'_', tuple_list, {'_', #{}}}},
                                {3, '_'},
                                {2, '_'},
                                {map, ?KV_OP_DEF(dict, new)},
                                {dict, ?MODULE},
                                {{lookup, 2}, {lookup, default}}
                            ], [])),
                    ?_assertEqual([{lookup, update_element}], ets:lookup(?MODULE, lookup))
                ]
            end}
    ].

-endif.