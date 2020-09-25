%%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 一颗分型树, 节点用列表实现
%%  数据从小到大排序
%%% 操作的数据越'靠前'效率越高, 适合热点数据增删和范围查询
%%% 使用场景为排行榜
%%% 改造方向, 需要实现
%%%     一, key不重复, 可以去掉重复key判断, lookup不再使用范围搜索
%%%     二, 叶节点直接使用record, 去掉构造部分
%%%     三, 逻辑最大最小值直接给定
%%%     四, 自动控制数量上限, 增加元素后检测, 剪枝直接去掉最后一个节点即可
%%% @end
%%%-------------------------------------------------------------------
-module(fractal_tree).
-author("dominic").

-include("util.hrl").

%% API
-export([new/0, lookup/2, store/3, erase/2, fold/3, fold/5]).

%% 以下简称 fractal_tree_origin -> fto

%% 对比测试过, 这组数据比较好
-define(FTO_AVG_NUM, 8).% 子节点平均数量
-define(FTO_DIVIDE_NUM, ?FTO_AVG_NUM * 2).% 分裂数量
-define(FTO_MERGE_NUM, ?FTO_AVG_NUM).% 合并数量

-define(FTO_MAX, max).% 逻辑最大值
-define(FTO_MIN, min).% 逻辑最小值

-define(FTO_FOLD_RANGE_KEY, undefined).% 遍历时逻辑用, 上一个key值, 针对key相同时的优化

%% 父节点
-record(fto_root, {
    children = [],% [#fto_child{}]
    num = 0
}).

%% 子节点
-record(fto_child, {
    high = 2,% 第一层是叶节点, 所以高度从2开始
    max = ?FTO_MAX,% 节点元素的最大值, 最右边叶节点key
    min = ?FTO_MIN,% 节点元素的最小值, 最左边叶节点key
    children = [],% [#ft_leaf{}|#fto_child{}]
    num = 0
}).

%% 叶节点
-record(fto_leaf, {
    key,% 索引的key, 节点按照该值升序排序
    private_key,% 唯一key, 区分相同key
    data% 数据
}).


%% @doc 创建分形树
-spec new() -> #fto_root{}.
new() ->
    #fto_root{num = 1, children = [#fto_child{}]}.


%% @doc 根据key查找
-spec lookup(term(), #fto_root{}) -> [Data :: term()].
lookup(Key, Root) ->
    fold(fun(Data, Acc) -> [Data | Acc] end, [], Root, Key, Key).


%% @doc 保存一个数据<br/>
%% 因为key可能会变所以必须传旧数据
-spec store(undefined|term(), term(), #fto_root{}) -> #fto_root{}.
store(undefined, NewData, #fto_root{num = ChildrenNum, children = Child} = Root) ->
    %% 存一个新数据
    NewLeaf = data2leaf(NewData),
    {IsDivide, Child1} = store_new(NewLeaf#fto_leaf.key, NewLeaf, Child, []),
    case IsDivide of
        true -> check_root_divide(ChildrenNum + 1, Child1);
        false -> Root#fto_root{children = Child1}
    end;
store(OldData, NewData, #fto_root{children = Child} = Root) ->
    OldLeaf = data2leaf(OldData),
    NewLeaf = data2leaf(NewData),
    case OldLeaf#fto_leaf.key == NewLeaf#fto_leaf.key of
        true ->
            %% key值不变, 仅更新数据
            case store_update(NewLeaf#fto_leaf.key, NewLeaf, Child, []) of
                false ->
                    throw(badarg);
                Child1  ->
                    Root#fto_root{children = Child1}
            end;
        false ->
            %% 先删除再插入
            #fto_root{num = ChildrenNum1, children = Child1} = Root1 = erase(OldLeaf, Root),
            {IsDivide, Child2} = store_new(NewLeaf#fto_leaf.key, NewLeaf, Child1, []),
            case IsDivide of
                true -> check_root_divide(ChildrenNum1 + 1, Child2);
                false -> Root1#fto_root{children = Child2}
            end
    end.

%% 插入一个新的叶节点
store_new(Key, Leaf, [#fto_child{high = 2, min = Min, max = Max, children = LeafList, num = LeafNum} = H | T], Reverse) ->
    IsBigger = is_bigger(Key, Max),
    case IsBigger andalso T =/= [] of
        true ->% 存在更大的值
            store_new(Key, Leaf, T, [H | Reverse]);
        false ->% 在这个节点插入
            LeafList1 = store_new_leaf(Key, Leaf, LeafList, []),
            {Max1, Min1} = store_calc_max_min(IsBigger, Key, Max, Min, T),
            check_divide(LeafNum, 2, Max1, Min1, LeafList1, H, T, Reverse)
    end;
store_new(Key, Leaf, [#fto_child{high = High, min = Min, max = Max, children = Children, num = ChildrenNum} = H | T], Reverse) ->
    IsBigger = is_bigger(Key, Max),
    case IsBigger andalso T =/= [] of
        true ->% 存在更大的值
            store_new(Key, Leaf, T, [H | Reverse]);
        false ->% 在这个节点插入
            {IsDivide, Children1} = store_new(Key, Leaf, Children, []),
            {Max1, Min1} = store_calc_max_min(IsBigger, Key, Max, Min, T),
            case IsDivide of
                true ->
                    check_divide(ChildrenNum, High, Max1, Min1, Children1, H, T, Reverse);
                false ->
                    H1 = H#fto_child{min = Min1, children = Children1},
                    {false, lists:reverse(Reverse, [H1 | T])}
            end
    end.

%% 插入新的叶节点
store_new_leaf(_Key, Leaf, [], Reverse) ->
    lists:reverse(Reverse, [Leaf]);
store_new_leaf(Key, Leaf, [H | T] = L, Reverse) ->
    case is_bigger(Key, H#fto_leaf.key) of
        true ->
            store_new_leaf(Key, Leaf, T, [H | Reverse]);
        false ->
            lists:reverse(Reverse, [Leaf | L])
    end.

%% 计算新节点最大最小值
store_calc_max_min(IsBigger, Key, Max, Min, T) ->
    Min1 =
        case is_bigger(Min, Key) of
            true -> Key;
            false -> Min
        end,
    Max1 =
        case T == [] andalso IsBigger of
            true -> Key;
            false -> Max
        end,
    {Max1, Min1}.

%% 分裂节点
check_divide(ChildrenNum, High, Max1, Min1, Children, H, T, Reverse) ->
    case ChildrenNum == ?FTO_DIVIDE_NUM - 1 of
        true ->% 分裂
            {Left, Right, LeftMax, RightMin} = divide(?FTO_AVG_NUM, Children, []),
            LeftChild = #fto_child{high = High, min = Min1, max = LeftMax, children = Left, num = ?FTO_AVG_NUM},
            RightChild = #fto_child{high = High, min = RightMin, max = Max1, children = Right, num = ?FTO_AVG_NUM},
            {true, lists:reverse(Reverse, [LeftChild, RightChild | T])};
        false ->
            H1 = H#fto_child{min = Min1, max = Max1, children = Children, num = ChildrenNum + 1},
            {false, lists:reverse(Reverse, [H1 | T])}
    end.


%% 高版本erl才支持, 好像这个更快
- if (?FTO_AVG_NUM == 8).

divide(_N, [L1, L2, L3, L4, L5, L6, L7, #fto_leaf{key = LeftMax} = L8, #fto_leaf{key = RightMin} = R1 | T], _Left) ->
    {[L1, L2, L3, L4, L5, L6, L7, L8], [R1 | T], LeftMax, RightMin};
divide(_N, [L1, L2, L3, L4, L5, L6, L7, #fto_child{max = LeftMax} = L8, #fto_child{min = RightMin} = R1 | T], _Left) ->
    {[L1, L2, L3, L4, L5, L6, L7, L8], [R1 | T], LeftMax, RightMin}.

-else.

divide(1, [#fto_leaf{key = LeftMax} = LeftLast, #fto_leaf{key = RightMin} = RightFirst | T], Left) ->
    {lists:reverse(Left, [LeftLast]), [RightFirst | T], LeftMax, RightMin};
divide(1, [#fto_child{max = LeftMax} = LeftLast, #fto_child{min = RightMin} = RightFirst | T], Left) ->
    {lists:reverse(Left, [LeftLast]), [RightFirst | T], LeftMax, RightMin};
divide(N, [H | T], Left) ->
    divide(N - 1, T, [H | Left]).

-endif.

%% 检查是否需要分裂父节点
check_root_divide(ChildrenNum, Child) ->
    case ChildrenNum == ?FTO_DIVIDE_NUM of
        true ->% 分裂
            #fto_child{high = High} = hd(Child),
            High1 = High + 1,
            {Left, Right, LeftMax, RightMin} = divide(?FTO_AVG_NUM, Child, []),
            LeftChild = #fto_child{high = High1, min = ?FTO_MIN, max = LeftMax, children = Left, num = ?FTO_AVG_NUM},
            RightChild = #fto_child{high = High1, min = RightMin, max = ?FTO_MAX, children = Right, num = ?FTO_AVG_NUM},
            #fto_root{num = 2, children = [LeftChild, RightChild]};
        false ->
            #fto_root{num = ChildrenNum, children = Child}
    end.

%% 仅更新
store_update(_Key, _Leaf, [], _Reverse) ->
    false;
store_update(Key, Leaf, [#fto_child{high = 2, max = Max, children = LeafList} = H | T], Reverse) ->
    IsBigger = is_bigger(Key, Max),
    case not IsBigger andalso lists:keymember(Leaf#fto_leaf.private_key, #fto_leaf.private_key, LeafList) of
        true ->% 在这个节点
            LeafList1 = lists:keyreplace(Leaf#fto_leaf.private_key, #fto_leaf.private_key, LeafList, Leaf),
            H1 = H#fto_child{children = LeafList1},
            lists:reverse(Reverse, [H1 | T]);
        false ->
            store_update(Key, Leaf, T, [H | Reverse])
    end;
store_update(Key, Leaf, [#fto_child{max = Max, children = Children} = H | T], Reverse) ->
    case is_bigger(Key, Max) of
        true ->
            store_update(Key, Leaf, T, [H | Reverse]);
        false ->% 在这个节点
            case store_update(Key, Leaf, Children, []) of
                false ->% 重复key
                    store_update1(Key, Leaf, T, [H | Reverse]);
                Children1 ->
                    H1 = H#fto_child{children = Children1},
                    lists:reverse(Reverse, [H1 | T])
            end
    end.

%% 存在重复key, 跨节点
store_update1(_Key, _Leaf, [], _Reverse) ->
    false;
store_update1(Key, Leaf, [#fto_child{high = 2, min = Min, children = LeafList} = H | T], Reverse) ->
    case is_bigger(Min, Key) of
        true ->% 不存在
            erlang:error(badarg);
        false ->
            case lists:keymember(Leaf#fto_leaf.private_key, #fto_leaf.private_key, LeafList) of
                true ->% 在这个节点
                    LeafList1 = lists:keyreplace(Leaf#fto_leaf.private_key, #fto_leaf.private_key, LeafList, Leaf),
                    H1 = H#fto_child{children = LeafList1},
                    lists:reverse(Reverse, [H1 | T]);
                false ->
                    store_update1(Key, Leaf, T, [H | Reverse])
            end
    end;
store_update1(Key, Leaf, [#fto_child{min = Min, children = Children} = H | T], Reverse) ->
    case is_bigger(Min, Key) of
        true ->% 不存在的key
            throw(badarg);
        false ->% 在这个节点
            case store_update1(Key, Leaf, Children, []) of
                false ->% 重复key
                    store_update1(Key, Leaf, T, [H | Reverse]);
                Children1 ->
                    H1 = H#fto_child{children = Children1},
                    lists:reverse(Reverse, [H1 | T])
            end
    end.


%% @doc 删除一个叶节点
-spec erase(term(), #fto_root{}) -> #fto_root{}.
erase(Data, #fto_root{num = ChildrenNum, children = Child} = Root) ->
    #fto_leaf{key = Key} = Leaf = data2leaf(Data),
    case erase(Key, Leaf, Child, []) of
        false ->% 不存在
            erlang:error(badarg);
        {false, Child1} ->% 没有发生合并
            Root#fto_root{children = Child1};
        {true, Child1} when ChildrenNum > 2 ->% 发生合并, 但是不需要降低高度
            #fto_root{num = ChildrenNum - 1, children = Child1};
        {true, Child1} ->% 降低高度
            Child2 = erase_reduce_high(Child1),
            #fto_root{num = 1, children = Child2}
    end.

erase(_Key, _Leaf, [], _Reverse) ->
    false;
erase(Key, Leaf, [#fto_child{high = 2, max = Max, num = LeafNum, children = LeafList} = H | T], Reverse) ->
    case is_bigger(Key, Max) of
        true ->
            erase(Key, Leaf, T, [H | Reverse]);
        false ->% 在这个节点
            case lists:keymember(Leaf#fto_leaf.private_key, #fto_leaf.private_key, LeafList) of
                true ->
                    LeafList1 = lists:keydelete(Leaf#fto_leaf.private_key, #fto_leaf.private_key, LeafList),
                    H1 = H#fto_child{num = LeafNum - 1, children = LeafList1},
                    erase_merge(H1, T, Reverse);
                false ->% key相同, 下个节点继续找
                    erase1(Key, Leaf, T, [H | Reverse])
            end
    end;
erase(Key, Leaf, [#fto_child{max = Max, children = Children, num = ChildrenNum} = H | T], Reverse) ->
    case is_bigger(Key, Max) of
        true ->
            erase(Key, Leaf, T, [H | Reverse]);
        false ->% 在这个节点
            case erase(Key, Leaf, Children, []) of
                false ->% 这个节点没找到, 下个节点再找
                    erase1(Key, Leaf, T, [H | Reverse]);
                {false, Children1} ->
                    H1 = H#fto_child{children = Children1},
                    {false, lists:reverse(Reverse, [H1 | T])};
                {true, Children1} ->% 尝试与相邻节点合并
                    H1 = H#fto_child{num = ChildrenNum - 1, children = Children1},
                    erase_merge(H1, T, Reverse)
            end
    end.

%% 已经找到过节点, 但是存在相同key
%% 这里如果key唯一的话, 可以优化得更简单
erase1(_Key, _Leaf, [], _Reverse) ->
    false;
erase1(Key, Leaf, [#fto_child{high = 2, min = Min, num = LeafNum, children = LeafList} = H | T], Reverse) ->
    case is_bigger(Min, Key) of
        true ->% 不存在
            erlang:error(badarg);
        false ->
            case lists:keymember(Leaf#fto_leaf.private_key, #fto_leaf.private_key, LeafList) of
                true ->
                    LeafList1 = lists:keydelete(Leaf#fto_leaf.private_key, #fto_leaf.private_key, LeafList),
                    H1 = H#fto_child{num = LeafNum - 1, children = LeafList1},
                    erase_merge(H1, T, Reverse);
                false ->
                    erase1(Key, Leaf, T, [H | Reverse])
            end
    end;
erase1(Key, Leaf, [#fto_child{min = Min, children = Children, num = ChildrenNum} = H | T], Reverse) ->
    case is_bigger(Min, Key) of
        true ->% 不存在
            erlang:error(badarg);
        false ->
            case erase1(Key, Leaf, Children, []) of
                false ->% 这个节点没找到, 下个节点再找
                    erase1(Key, Leaf, T, [H | Reverse]);
                {false, Children1} ->
                    H1 = H#fto_child{children = Children1},
                    {false, lists:reverse(Reverse, [H1 | T])};
                {true, Children1} ->% 尝试与相邻节点合并
                    H1 = H#fto_child{num = ChildrenNum - 1, children = Children1},
                    erase_merge(H1, T, Reverse)
            end
    end.

%% 尝试合并节点
erase_merge(#fto_child{num = 0}, T, Reverse) ->
    {true, lists:reverse(Reverse, T)};
erase_merge(#fto_child{num = CN1} = H1, [#fto_child{num = CN2} = H2 | T], Reverse) when CN1 + CN2 < ?FTO_MERGE_NUM ->
    {true, lists:reverse(Reverse, [merge(H1, H2) | T])};
erase_merge(#fto_child{num = CN2} = H2, T, [#fto_child{num = CN1} = H1 | Reverse]) when CN1 + CN2 < ?FTO_MERGE_NUM ->
    {true, lists:reverse(Reverse, [merge(H1, H2) | T])};
erase_merge(H, T, Reverse) ->
    {false, lists:reverse(Reverse, [H | T])}.

merge(Left, Right) ->
    #fto_child{high = High, children = Children1, num = ChildrenNum1, min = Min} = Left,
    #fto_child{children = Children2, num = ChildrenNum2, max = Max} = Right,
    #fto_child{
        high = High, children = Children1 ++ Children2,
        num = ChildrenNum1 + ChildrenNum2,
        min = Min, max = Max
    }.

%% 降低高度, 防止变成线性列表
erase_reduce_high([]) -> [#fto_child{}];
erase_reduce_high([#fto_child{high = 2}] = Child) -> Child;
erase_reduce_high([#fto_child{num = 1, children = Children}]) -> erase_reduce_high(Children);
erase_reduce_high([_] = Child) -> Child.

%% @doc 遍历tree
-spec fold(fun((Data, Acc)-> Acc1), Acc, #fto_root{}) -> Acc1
    when Data :: term(), Acc :: term(), Acc1 :: term().
fold(F, Acc, #fto_root{children = #fto_child{high = 2, children = []}}) when is_function(F, 2) ->
    Acc;
fold(F, Acc, #fto_root{children = Child}) ->
    case fold_child(F, Acc, Child) of
        ?FOLD_BREAK_1(Acc1) -> Acc1;
        Acc1 -> Acc1
    end.

fold_child(_F, Acc, []) ->
    Acc;
fold_child(F, Acc, [#fto_child{high = 2, children = LeafList} | T]) ->
    case fold_leaf(F, Acc, LeafList) of
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 -> fold_child(F, Acc1, T)
    end;
fold_child(F, Acc, [#fto_child{children = Children} | T]) ->
    case fold_child(F, Acc, Children) of
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 -> fold_child(F, Acc1, T)
    end.


%% @doc 根据范围遍历tree, 频繁使用的话, 同tree结构更优
-spec fold(fun((Data, Acc)-> Acc1), Acc, #fto_root{}, term(), term()) -> Acc1
    when Data :: term(), Acc :: term(), Acc1 :: term().
fold(F, Acc, #fto_root{children = #fto_child{high = 2, children = []}}, _Max, _Min) when is_function(F, 2) ->
    Acc;
fold(F, Acc, #fto_root{children = Child}, Max, Min) ->
    case fold_child_by_range(F, Acc, Child, Max, Min) of
        ?FOLD_BREAK_1(Acc1) -> Acc1;
        Acc1 -> Acc1
    end.

fold_child_by_range(_F, Acc, [], _Max, _Min) ->
    Acc;
fold_child_by_range(F, Acc, [#fto_child{max = ChildMax, high = 2, children = LeafList} | T], Max, Min) ->
    case is_bigger(Min, ChildMax) of
        true ->
            fold_child_by_range(F, Acc, T, Max, Min);
        false ->
            case fold_leaf_by_range(F, Acc, ?FTO_FOLD_RANGE_KEY, LeafList, Max, Min) of
                ?FOLD_BREAK_1(_Acc1) = Break -> Break;
                Acc1 -> fold_child_by_range1(F, Acc1, T, Max, Min)
            end
    end;
fold_child_by_range(F, Acc, [#fto_child{max = ChildMax, children = Children} | T], Max, Min) ->
    case is_bigger(Min, ChildMax) of
        true ->
            fold_child_by_range(F, Acc, T, Max, Min);
        false ->
            case fold_child_by_range(F, Acc, Children, Max, Min) of
                ?FOLD_BREAK_1(_Acc1) = Break -> Break;
                Acc1 -> fold_child_by_range1(F, Acc1, T, Max, Min)
            end
    end.

%% 此时已经找到节点
fold_child_by_range1(_F, Acc, [], _Max, _Min) ->
    Acc;
fold_child_by_range1(F, Acc, [#fto_child{min = ChildMin, high = 2, children = LeafList} | T], Max, Min) ->
    case is_bigger(ChildMin, Max) of
        true ->
            ?FOLD_BREAK_1(Acc);
        false ->
            case fold_leaf_by_range(F, Acc, ?FTO_FOLD_RANGE_KEY, LeafList, Max, Min) of
                ?FOLD_BREAK_1(_Acc1) = Break -> Break;
                Acc1 -> fold_child_by_range1(F, Acc1, T, Max, Min)
            end
    end;
fold_child_by_range1(F, Acc, [#fto_child{min = ChildMin, children = Children} | T], Max, Min) ->
    case is_bigger(ChildMin, Max) of
        true ->
            ?FOLD_BREAK_1(Acc);
        false ->
            case fold_child_by_range1(F, Acc, Children, Max, Min) of
                ?FOLD_BREAK_1(_Acc1) = Break -> Break;
                Acc1 -> fold_child_by_range1(F, Acc1, T, Max, Min)
            end
    end.


fold_leaf(F, Acc, [Leaf | T]) ->
    case F(Leaf#fto_leaf.data, Acc) of
        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc);
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 -> fold_leaf(F, Acc1, T)
    end;
fold_leaf(_F, Acc, []) -> Acc.


fold_leaf_by_range(_F, Acc, _LastKey, [], _Max, _Min) ->
    Acc;
fold_leaf_by_range(F, Acc, Key, [#fto_leaf{key = Key} = Leaf | T], Max, Min) ->
    case F(Leaf#fto_leaf.data, Acc) of
        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc);
        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
        Acc1 -> fold_leaf_by_range(F, Acc1, Key, T, Max, Min)
    end;
fold_leaf_by_range(F, Acc, LastKey, [#fto_leaf{key = Key} = Leaf | T], Max, Min) ->
    case is_bigger(Min, Key) of
        true ->
            fold_leaf_by_range(F, Acc, LastKey, T, Max, Min);
        false ->
            case is_bigger(Key, Max) of
                true ->
                    ?FOLD_BREAK_1(Acc);
                false ->
                    case F(Leaf#fto_leaf.data, Acc) of
                        ?FOLD_BREAK -> ?FOLD_BREAK_1(Acc);
                        ?FOLD_BREAK_1(_Acc1) = Break -> Break;
                        Acc1 -> fold_leaf_by_range(F, Acc1, Key, T, Max, Min)
                    end
            end
    end.

%% 实现逻辑最大值和最小值
%% 根据具体场景可以优化成直接比较
is_bigger(?FTO_MAX, _) -> true;
is_bigger(_, ?FTO_MAX) -> false;
is_bigger(?FTO_MIN, _) -> false;
is_bigger(_, ?FTO_MIN) -> true;
is_bigger(Key1, Key2) -> Key1 > Key2.

-ifdef(TEST).
data2leaf(Data) ->
    #fto_leaf{key = Data rem 10, private_key = Data, data = Data}.
-else.
%% data数据转换成叶节点
%% 根据具体场景可以优化成record代替
data2leaf(Data) ->
    #fto_leaf{key = erlang:phash2(Data) rem 1000, private_key = erlang:phash2(Data), data = Data}.
-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% test %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

base_test_() ->
    D100 =
        lists:foldl(fun(N, Acc) ->
            fractal_tree:store(undefined, N, Acc) end,
            fractal_tree:new(), lists:seq(1, 100)),
    List100 =
        fractal_tree:fold(fun(N, Acc) ->
            [N | Acc]
                                 end, [], D100),
    D80 =
        lists:foldl(fun(N, Acc) ->
            fractal_tree:erase(N, Acc)
                    end, D100, lists:seq(1, 20)),
    List80 =
        fractal_tree:fold(fun(N, Acc) ->
            [N | Acc]
                                 end, [], D80),
    [
        ?_assertEqual([N2 + N1 * 10 || N2 <- lists:seq(9, 1, -1) ++ [10], N1 <- lists:seq(0, 9)], List100),
        ?_assertEqual([N2 + N1 * 10 || N2 <- lists:seq(9, 1, -1) ++ [10], N1 <- lists:seq(2, 9)], List80),
        ?_assertEqual([N * 10 + 1 || N <- lists:seq(0, 9)], fractal_tree:lookup(1, D100)),
        ?_assertEqual([N * 10 + 1 || N <- lists:seq(2, 9)], fractal_tree:lookup(1, D80))
    ].

-endif.


