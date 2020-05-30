%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 5月 2020 16:40
%%%-------------------------------------------------------------------
-module(exia_tree).
-author("dominic").

-include("exia.hrl").
-include("util.hrl").

-compile(inline).

-export([new/0, lookup/2, store/3, erase/2, fold/3, fold_by_range/5]).

-define(EXIA_TREE_EXPIRE_SIZE, 8).% 期望每个节点存储数据数量
-define(EXIA_TREE_DIVIDE_SIZE, ?EXIA_TREE_EXPIRE_SIZE * 2).% 分裂数量

-record(exia_tree, {
    deep = 1,
    grandchildren_num = 0,
    children_num = 0,
    min = ?EXIA_TREE_MIN,
    max = ?EXIA_TREE_MAX,
    children = []% [#exit_tree{}|#exia_ie{}]
}).


%% @doc 创建一个tree索引, 数据从小到大排序
-spec new() -> [#exia_tree{}].
new() ->
    [#exia_tree{}].


%% @doc 根据key查找
lookup(Key, Tree) ->
    fold_by_range(fun(E, Acc) -> [E | Acc] end, [], Tree, Key, Key).


%% @doc 保存一个element
-spec store(#exia_ie{}|undefined, #exia_ie{}, [#exia_tree{}]) -> [#exia_tree{}].
store(OldElement, NewElement, Tree) ->
    if
        OldElement == undefined -> store_new(NewElement, Tree);
        OldElement#exia_ie.key > NewElement#exia_ie.key ->;
        OldElement#exia_ie.key < NewElement#exia_ie.key ->;
        true ->
    end.


%% @doc 删除一个element
-spec erase(#exia_ie{}, [#exia_tree{}]) -> [#exia_tree{}].
erase(Element, Tree) ->
.

store_new(Key, Element, [#exia_tree{
    deep = 1, max = Max,
    children_num = ChildrenNum, children = ElementList
} = H | T], Reverse) ->
    case is_bigger(Key, Max) of
        true ->
            store_new(Key, Element, T, [H | T]);
        _ ->
    
    end.

merge_size(2) ->

%% @doc 遍历tree
-spec fold(fun((#exia_ie{}, Acc) -> Acc1), Acc, #exia_tree{}) -> Acc1
    when Acc :: term(), Acc1 :: term().
fold(F, Acc, Tree) ->
.


%% @doc 根据范围遍历tree
-spec fold_by_range(fun((#exia_ie{}, Acc)-> Acc1), Acc, #exia_tree{}, term(), term()) -> Acc1
    when Acc :: term(), Acc1 :: term().
fold_by_range(F, Acc, Tree, Max, Min) ->
.

is_bigger(?EXIA_TREE_MAX, _) -> true;
is_bigger(_, ?EXIA_TREE_MAX) -> false;
is_bigger(?EXIA_TREE_MIN, _) -> false;
is_bigger(_, ?EXIA_TREE_MIN) -> true;
is_bigger(Key1, Key2) -> Key1 > Key2.

