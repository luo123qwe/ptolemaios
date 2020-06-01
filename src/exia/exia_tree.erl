%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(exia_tree).
-author("dominic").

-include("util.hrl").
-include("exia.hrl").

-compile(inline).

%% API
-export([new/0, lookup/2, store/3, erase/2, fold/3]).

-define(EXIA_TREE_AVG_NUM, 8).
-define(EXIA_TREE_DIVIDE_NUM, 16).

-record(exia_tree_root, {
    children = [],% [#exia_tree{}]
    children_num = 0
}).

-record(exia_tree, {
    floor = 2,% 第一层是[#exia_ie{}]
    max = ?EXIA_TREE_MAX,
    min = ?EXIA_TREE_MIN,
    children = [],% [#exia_ie{}|#exia_tree{}]
    children_num = 0
}).

-type root() :: #exia_tree_root{}.
-export_type([root/0]).


%% @doc 创建一个root
-spec new() -> root().
new() ->
    #exia_tree_root{children_num = 1, children = [#exia_tree{}]}.


%% @doc 根据key查找
-spec lookup(term(), root()) -> [#exia_ie{}].
lookup(Key, Root) ->
    fold_by_range(fun(E, Acc) -> [E | Acc] end, [], Root, Key, Key).


%% @doc 保存一个element
-spec store(#exia_ie{}|undefined, #exia_ie{}, root()) -> root().
store(OldElement, NewElement, #exia_tree_root{children_num = ChildrenNum, children = Tree} = Root) ->
    if
        OldElement == undefined ->% 新element
            {IsDivide, Tree1} = store_new(NewElement#exia_ie.key, NewElement, Tree, []),
            case IsDivide of
                true -> check_root_divide(ChildrenNum + 1, Tree1);
                false -> Root#exia_tree_root{children = Tree1}
            end;
        OldElement#exia_ie.key == NewElement#exia_ie.key ->% 索引值没变
            Tree1 = store_update(NewElement#exia_ie.key, NewElement, Tree, []),
            Root#exia_tree_root{children = Tree1};
        true ->
            Tree1 = erase(OldElement, Tree),
            {IsDivide, Tree2} = store_new(NewElement#exia_ie.key, NewElement, Tree1, []),
            case IsDivide of
                true -> check_root_divide(ChildrenNum + 1, Tree2);
                false -> Root#exia_tree_root{children = Tree1}
            end
    end.

store_new(Key, Element, [#exia_tree{floor = 2, min = Min, max = Max, children = ElementList, children_num = ElementNum} = H | T], Reverse) ->
    IsBigger = is_bigger(Key, Max),
    case IsBigger andalso T =/= [] of
        true ->
            store_new(Key, Element, T, [H | Reverse]);
        false ->
            ElementList1 = store_new_element(Key, Element, ElementList, []),
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
            case ElementNum == ?EXIA_TREE_DIVIDE_NUM - 1 of
                true ->% 分裂
                    {Left, Right, LeftMax, RightMin} = divide(?EXIA_TREE_AVG_NUM, ElementList1, []),
                    LeftTree = #exia_tree{floor = 2, min = Min1, max = LeftMax, children = Left, children_num = ?EXIA_TREE_AVG_NUM},
                    RightTree = #exia_tree{floor = 2, min = RightMin, max = Max1, children = Right, children_num = ?EXIA_TREE_AVG_NUM},
                    {true, lists:reverse(Reverse, [LeftTree, RightTree | T])};
                false ->
                    H1 = H#exia_tree{min = Min1, max = Max1, children = ElementList1, children_num = ElementNum + 1},
                    {false, lists:reverse(Reverse, [H1 | T])}
            end
    end;
store_new(Key, Element, [#exia_tree{floor = Floor, min = Min, max = Max, children = Children, children_num = ChildrenNum} = H | T], Reverse) ->
    IsBigger = is_bigger(Key, Max),
    case IsBigger andalso T =/= [] of
        true ->
            store_new(Key, Element, T, [H | Reverse]);
        false ->
            {IsDivide, Children1} = store_new(Key, Element, Children, []),
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
            case IsDivide of
                true ->
                    case ChildrenNum == ?EXIA_TREE_DIVIDE_NUM - 1 of
                        true ->% 分裂
                            {Left, Right, LeftMax, RightMin} = divide(?EXIA_TREE_AVG_NUM, Children1, []),
                            LeftTree = #exia_tree{floor = Floor, min = Min1, max = LeftMax, children = Left, children_num = ?EXIA_TREE_AVG_NUM},
                            RightTree = #exia_tree{floor = Floor, min = RightMin, max = Max1, children = Right, children_num = ?EXIA_TREE_AVG_NUM},
                            {true, lists:reverse(Reverse, [LeftTree, RightTree | T])};
                        false ->
                            H1 = H#exia_tree{min = Min1, max = Max1, children = Children1, children_num = ChildrenNum + 1},
                            {false, lists:reverse(Reverse, [H1 | T])}
                    end;
                false ->
                    H1 = H#exia_tree{
                        min = Min1,
                        children = Children1
                    },
                    {false, lists:reverse(Reverse, [H1 | T])}
            end
    end.

store_new_element(_Key, Element, [], Reverse) ->
    lists:reverse(Reverse, [Element]);
store_new_element(Key, Element, [H | T] = L, Reverse) ->
    case is_bigger(Key, H#exia_ie.key) of
        true ->
            store_new_element(Key, Element, T, [H | Reverse]);
        false ->
            lists:reverse(Reverse, [Element | L])
    end.

- if (?EXIA_TREE_AVG_NUM == 8).

divide(_N, [L1, L2, L3, L4, L5, L6, L7, #exia_ie{key = LeftMax} = L8, #exia_ie{key = RightMin} = R1 | T], _Left) ->
    {[L1, L2, L3, L4, L5, L6, L7, L8], [R1 | T], LeftMax, RightMin};
divide(_N, [L1, L2, L3, L4, L5, L6, L7, #exia_tree{max = LeftMax} = L8, #exia_tree{min = RightMin} = R1 | T], _Left) ->
    {[L1, L2, L3, L4, L5, L6, L7, L8], [R1 | T], LeftMax, RightMin}.

-else.

divide(1, [#exia_ie{key = LeftMax} = LeftLast, #exia_ie{key = RightMin} = RightFirst | T], Left) ->
    {lists:reverse(Left, [LeftLast]), [RightFirst | T], LeftMax, RightMin};
divide(1, [#exia_tree{max = LeftMax} = LeftLast, #exia_tree{min = RightMin} = RightFirst | T], Left) ->
    {lists:reverse(Left, [LeftLast]), [RightFirst | T], LeftMax, RightMin};
divide(N, [H | T], Left) ->
    divide(N - 1, T, [H | Left]).

-endif.

check_root_divide(ChildrenNum, Tree) ->
    case ChildrenNum == ?EXIA_TREE_DIVIDE_NUM of
        true ->% 分裂
            #exia_tree{floor = Floor} = hd(Tree),
            Floor1 = Floor + 1,
            {Left, Right, LeftMax, RightMin} = divide(?EXIA_TREE_AVG_NUM, Tree, []),
            LeftTree = #exia_tree{floor = Floor1, min = ?EXIA_TREE_MIN, max = LeftMax, children = Left, children_num = ?EXIA_TREE_AVG_NUM},
            RightTree = #exia_tree{floor = Floor1, min = RightMin, max = ?EXIA_TREE_MAX, children = Right, children_num = ?EXIA_TREE_AVG_NUM},
            [LeftTree, RightTree];
        false ->
            #exia_tree_root{children_num = ChildrenNum, children = Tree}
    end.

store_update(_Key, _Element, [], _Reverse) ->
    erlang:error(badarg);
store_update(Key, Element, [#exia_tree{floor = 2, max = Max, children = ElementList} = H | T], Reverse) ->
    case not is_bigger(Key, Max) andalso lists:keymember(Element#exia_ie.private_key, #exia_ie.private_key, ElementList) of
        true ->
            ElementList1 = lists:keystore(Element#exia_ie.private_key, #exia_ie.private_key, ElementList, Element),
            H1 = H#exia_tree{children = ElementList1},
            lists:reverse(Reverse, [H1 | T]);
        false ->
            store_update(Key, Element, T, [H | Reverse])
    end;
store_update(Key, Element, [#exia_tree{max = Max, children = Children} = H | T], Reverse) ->
    case is_bigger(Key, Max) of
        true ->
            store_update(Key, Element, T, [H | Reverse]);
        false ->
            Children1 = store_update(Key, Element, Children, []),
            H1 = H#exia_tree{children = Children1},
            lists:reverse(Reverse, [H1 | T])
    end.


%% @doc 删除一个element
-spec erase(#exia_ie{}, root()) -> root().
erase(#exia_ie{key = Key} = Element, #exia_tree_root{children_num = ChildrenNum, children = Tree} = Root) ->
    case erase(Key, Element, Tree, []) of
        false -> erlang:error(badarg);
        {false, Tree1} -> Root#exia_tree_root{children = Tree1};
        {true, Tree1} when ChildrenNum > 2 ->
            #exia_tree_root{children_num = ChildrenNum - 1, children = Tree1};
        {true, Tree1} ->
            Tree2 = check_erase_tree(Tree1),
            #exia_tree_root{children_num = 1, children = Tree2}
    end.

erase(_Key, _Element, [], _Reverse) ->
    false;
erase(Key, Element, [#exia_tree{floor = 2, max = Max, children_num = ElementNum, children = ElementList} = H | T], Reverse) ->
    case is_bigger(Key, Max) of
        true ->
            erase(Key, Element, T, [H | Reverse]);
        false ->
            case lists:keymember(Element#exia_ie.private_key, #exia_ie.private_key, ElementList) of
                true ->
                    ElementList1 = lists:keydelete(Element#exia_ie.private_key, #exia_ie.private_key, ElementList),
                    H1 = H#exia_tree{children_num = ElementNum - 1, children = ElementList1},
                    merge(H1, T, Reverse);
                false ->
                    erase1(Key, Element, T, [H | Reverse])
            end
    end;
erase(Key, Element, [#exia_tree{max = Max, min = Min, children = Children, children_num = ChildrenNum} = H | T], Reverse) ->
    case is_bigger(Key, Max) of
        true ->
            case is_bigger(Min, Key) of
                true ->
                    erlang:error(badarg);
                _ ->
                    erase(Key, Element, T, [H | Reverse])
            end;
        false ->
            case erase(Key, Element, Children, []) of
                false ->
                    erase1(Key, Element, T, [H | Reverse]);
                {false, Children1} ->
                    H1 = H#exia_tree{children = Children1},
                    {false, lists:reverse(Reverse, [H1 | T])};
                {true, Children1} ->% 尝试与相邻节点合并
                    H1 = H#exia_tree{children_num = ChildrenNum - 1, children = Children1},
                    merge(H1, T, Reverse)
            end
    end.

erase1(_Key, _Element, [], _Reverse) ->
    false;
erase1(Key, Element, [#exia_tree{floor = 2, min = Min, children_num = ElementNum, children = ElementList} = H | T], Reverse) ->
    case is_bigger(Min, Key) of
        true ->
            erlang:error(badarg);
        false ->
            case lists:keymember(Element#exia_ie.private_key, #exia_ie.private_key, ElementList) of
                true ->
                    ElementList1 = lists:keydelete(Element#exia_ie.private_key, #exia_ie.private_key, ElementList),
                    H1 = H#exia_tree{children_num = ElementNum - 1, children = ElementList1},
                    merge(H1, T, Reverse);
                false ->
                    erase1(Key, Element, T, [H | Reverse])
            end
    end;
erase1(Key, Element, [#exia_tree{min = Min, children = Children, children_num = ChildrenNum} = H | T], Reverse) ->
    case is_bigger(Min, Key) of
        true ->
            erlang:error(badarg);
        false ->
            case erase1(Key, Element, Children, []) of
                false ->
                    erase1(Key, Element, T, [H | Reverse]);
                {false, Children1} ->
                    H1 = H#exia_tree{children = Children1},
                    {false, lists:reverse(Reverse, [H1 | T])};
                {true, Children1} ->% 尝试与相邻节点合并
                    H1 = H#exia_tree{children_num = ChildrenNum - 1, children = Children1},
                    merge(H1, T, Reverse)
            end
    end.

merge(#exia_tree{children_num = 0}, T, Reverse) ->
    {true, lists:reverse(Reverse, T)};
merge(#exia_tree{children_num = CN1} = H1, [#exia_tree{children_num = CN2} = H2 | T], Reverse) when CN1 + CN2 < ?EXIA_TREE_DIVIDE_NUM ->
    {true, lists:reverse(Reverse, [merge(H1, H2) | T])};
merge(#exia_tree{children_num = CN2} = H2, T, [#exia_tree{children_num = CN1} = H1 | Reverse]) when CN1 + CN2 < ?EXIA_TREE_DIVIDE_NUM ->
    {true, lists:reverse(Reverse, [merge(H1, H2) | T])};
merge(H, T, Reverse) ->
    {false, lists:reverse(Reverse, [H | T])}.

merge(Left, Right) ->
    #exia_tree{floor = Floor, children = Children1, children_num = ChildrenNum1, min = Min} = Left,
    #exia_tree{children = Children2, children_num = ChildrenNum2, max = Max} = Right,
    #exia_tree{
        floor = Floor, children = Children1 ++ Children2,
        children_num = ChildrenNum1 + ChildrenNum2,
        min = Min, max = Max
    }.

check_erase_tree([]) -> [#exia_tree{}];
check_erase_tree([#exia_tree{floor = 2}] = Tree) -> Tree;
check_erase_tree([#exia_tree{children_num = 1, children = Children}]) -> check_erase_tree(Children);
check_erase_tree([_] = Tree) -> Tree.

%% @doc 遍历tree
-spec fold(fun((#exia_ie{}, Acc)-> Acc1), Acc, root()) -> Acc1
    when Acc :: term(), Acc1 :: term().
fold(F, Acc, #exia_tree_root{children = #exia_tree{floor = 2, children = []}}) when is_function(F, 2) ->
    Acc;
fold(F, Acc, #exia_tree_root{children = Tree}) ->
    case fold_tree(F, Acc, Tree) of
        ?UTIL_FOLD_BREAK(Acc1) -> Acc1;
        Acc1 -> Acc1
    end.

fold_tree(_F, Acc, []) ->
    Acc;
fold_tree(F, Acc, [#exia_tree{floor = 2, children = ElementList} | T]) ->
    case fold_element(F, Acc, ElementList) of
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_tree(F, Acc1, T)
    end;
fold_tree(F, Acc, [#exia_tree{children = Children} | T]) ->
    case fold_tree(F, Acc, Children) of
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_tree(F, Acc1, T)
    end.


%% @doc 根据范围遍历tree, 频繁使用的话, 同tree结构更优
-spec fold_by_range(fun((#exia_ie{}, Acc)-> Acc1), Acc, root(), term(), term()) -> Acc1
    when Acc :: term(), Acc1 :: term().
fold_by_range(F, Acc, #exia_tree_root{children = #exia_tree{floor = 2, children = []}}, _Max, _Min) when is_function(F, 2) ->
    Acc;
fold_by_range(F, Acc, #exia_tree_root{children = Tree}, Max, Min) ->
    case fold_tree_by_range(F, Acc, Tree, Max, Min) of
        ?UTIL_FOLD_BREAK(Acc1) -> Acc1;
        Acc1 -> Acc1
    end.

fold_tree_by_range(_F, Acc, [], _Max, _Min) ->
    Acc;
fold_tree_by_range(F, Acc, [#exia_tree{max = TreeMax, floor = 2, children = ElementList} | T], Max, Min) ->
    case is_bigger(Min, TreeMax) of
        true ->
            fold_tree_by_range(F, Acc, T, Max, Min);
        false ->
            case fold_element_by_range(F, Acc, ?EXIA_TREE_KEY, ElementList, Max, Min) of
                ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
                Acc1 -> fold_tree_by_range1(F, Acc1, T, Max, Min)
            end
    end;
fold_tree_by_range(F, Acc, [#exia_tree{max = TreeMax, children = Children} | T], Max, Min) ->
    case is_bigger(Min, TreeMax) of
        true ->
            fold_tree_by_range(F, Acc, T, Max, Min);
        false ->
            case fold_tree_by_range(F, Acc, Children, Max, Min) of
                ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
                Acc1 -> fold_tree_by_range1(F, Acc1, T, Max, Min)
            end
    end.

fold_tree_by_range1(_F, Acc, [], _Max, _Min) ->
    Acc;
fold_tree_by_range1(F, Acc, [#exia_tree{min = TreeMin, floor = 2, children = ElementList} | T], Max, Min) ->
    case is_bigger(TreeMin, Max) of
        true ->
            ?UTIL_FOLD_BREAK(Acc);
        false ->
            case fold_element_by_range(F, Acc, ?EXIA_TREE_KEY, ElementList, Max, Min) of
                ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
                Acc1 -> fold_tree_by_range1(F, Acc1, T, Max, Min)
            end
    end;
fold_tree_by_range1(F, Acc, [#exia_tree{min = TreeMin, children = Children} | T], Max, Min) ->
    case is_bigger(TreeMin, Max) of
        true ->
            ?UTIL_FOLD_BREAK(Acc);
        false ->
            case fold_tree_by_range1(F, Acc, Children, Max, Min) of
                ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
                Acc1 -> fold_tree_by_range1(F, Acc1, T, Max, Min)
            end
    end.


fold_element(F, Acc, [Element | T]) ->
    case F(Element, Acc) of
        ?UTIL_FOLD_BREAK -> ?UTIL_FOLD_BREAK(Acc);
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_element(F, Acc1, T)
    end;
fold_element(_F, Acc, []) -> Acc.


fold_element_by_range(_F, Acc, _LastKey, [], _Max, _Min) ->
    Acc;
fold_element_by_range(F, Acc, Key, [#exia_ie{key = Key} = Element | T], Max, Min) ->
    case F(Element, Acc) of
        ?UTIL_FOLD_BREAK -> ?UTIL_FOLD_BREAK(Acc);
        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
        Acc1 -> fold_element_by_range(F, Acc1, Key, T, Max, Min)
    end;
fold_element_by_range(F, Acc, LastKey, [#exia_ie{key = Key} = Element | T], Max, Min) ->
    case is_bigger(Min, Key) of
        true ->
            fold_element_by_range(F, Acc, LastKey, T, Max, Min);
        false ->
            case is_bigger(Key, Max) of
                true ->
                    ?UTIL_FOLD_BREAK(Acc);
                false ->
                    case F(Element, Acc) of
                        ?UTIL_FOLD_BREAK -> ?UTIL_FOLD_BREAK(Acc);
                        ?UTIL_FOLD_BREAK(_Acc1) = Break -> Break;
                        Acc1 -> fold_element_by_range(F, Acc1, Key, T, Max, Min)
                    end
            end
    end.


is_bigger(?EXIA_TREE_MAX, _) -> true;
is_bigger(_, ?EXIA_TREE_MAX) -> false;
is_bigger(?EXIA_TREE_MIN, _) -> false;
is_bigger(_, ?EXIA_TREE_MIN) -> true;
is_bigger(Key1, Key2) -> Key1 > Key2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% test %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

tree_test_() ->
    D100 =
        lists:foldl(fun(N, Acc) ->
            exia_tree:store(undefined, #exia_ie{private_key = N, key = N rem 10, record = {N, N rem 10}}, Acc) end,
            exia_tree:new(), lists:seq(1, 100)),
    List100 =
        exia_tree:fold(fun(Element, Acc) ->
            [Element | Acc]
                       end, [], D100),
    D80 =
        lists:foldl(fun(N, Acc) ->
            exia_tree:erase(#exia_ie{private_key = N, key = N rem 10, record = {N, N rem 10}}, Acc)
                    end, D100, lists:seq(1, 20)),
    List80 =
        exia_tree:fold(fun(Element, Acc) ->
            [Element | Acc]
                       end, [], D80),
    [
        ?_assertEqual([#exia_ie{private_key = N, key = N rem 10, record = {N, N rem 10}} || N <- lists:seq(1, 100)],
            lists:keysort(#exia_ie.private_key, List100)),
        ?_assertEqual([#exia_ie{private_key = N, key = N rem 10, record = {N, N rem 10}} || N <- lists:seq(21, 100)],
            lists:keysort(#exia_ie.private_key, List80)),
        ?_assertEqual([#exia_ie{private_key = N * 10 + 1, key = 1, record = {N * 10 + 1, 1}} || N <- lists:seq(0, 9)],
            exia_tree:lookup(1, D100)),
        ?_assertEqual([#exia_ie{private_key = N * 10 + 1, key = 1, record = {N * 10 + 1, 1}} || N <- lists:seq(2, 9)],
            exia_tree:lookup(1, D80))
    ].

-endif.


