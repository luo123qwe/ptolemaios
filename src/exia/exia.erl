%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 索引
%%% @end
%%%-------------------------------------------------------------------
-module(exia).
-author("dominic").

-include("exia.hrl").

-compile(inline).

%% 索引操作
-export([new/1, add/4, remove/3]).

%% 数据操作
-export([lookup/4, lookup/5, store/3, erase/2, fold/5, fold_by_range/7]).

-type key() :: RecordPos :: integer()|[RecordPos :: integer()].
-type alias() :: atom().
-type element_key() :: key()|alias().

%% @doc 创建一个新的索引结构
-spec new(key()) -> #exia{}.
new(PrivateKey) ->
    #exia{private_key = PrivateKey}.


%% @doc 添加一个索引
-spec add(integer(), key(), alias(), #exia{}) -> #exia{}.
add(ExiaRecordPos, Key, Alias, Exia) when (is_integer(Key) orelse is_list(Key)), is_atom(Alias) ->
    ElementList = element(ExiaRecordPos, Exia),
    case
        lists:keymember(Key, #exia_i.key, ElementList)
            orelse lists:keymember(Alias, #exia_i.alias, ElementList)
    of
        true ->
            erlang:throw(exist);
        false ->
            NewElement = #exia_i{key = Key, alias = Alias, index = add_1(ExiaRecordPos)},
            setelement(ExiaRecordPos, Exia, [NewElement | ElementList])
    end.

add_1(#exia.tree) ->
    exia_tree:new();
add_1(#exia.dict) ->
    exia_dict:new();
add_1(_) ->
    erlang:error(badarg).


%% @doc 移除一个索引
-spec remove(integer(), element_key(), #exia{}) -> #exia{}.
remove(ExiaRecordPos, ElementKey, Exia) ->
    ElementList = element(ExiaRecordPos, Exia),
    ElementList1 = delete_element(ElementKey, ElementList),
    setelement(ExiaRecordPos, Exia, ElementList1).


%% @doc 查找数据, 返回record列表
-spec lookup(integer(), element_key(), key(), #exia{}) -> [#exia_ie{}].
lookup(ExiaRecordPos, ElementKey, Key, ExiaIndex) ->
    ElementList = element(ExiaRecordPos, ExiaIndex),
    case get_element(ElementKey, ElementList) of
        false ->
            [];
        #exia_i{index = Index} ->
            lookup_1(ExiaRecordPos, Key, Index)
    end.

lookup_1(#exia.tree, Key, Tree) ->
    exia_tree:lookup(Key, Tree);
lookup_1(#exia.dict, Key, Dict) ->
    exia_dict:lookup(Key, Dict);
lookup_1(_, _, _) ->
    erlang:error(badarg).


%% @doc 根据某个索引查找数据
-spec lookup(integer(), element_key(), key(), key(), #exia{}) -> #exia_ie{}|undefined.
lookup(ExiaRecordPos, ElementKey, Key, PrivateKey, ExiaIndex) ->
    lookup_private(PrivateKey, ExiaIndex#exia.private_key, lookup(ExiaRecordPos, ElementKey, Key, ExiaIndex)).

lookup_private(_PrivateKey, _KeyIndexList, []) ->
    undefined;
lookup_private(PrivateKey, KeyIndexList, [#exia_ie{record = Record} = H | T]) ->
    case PrivateKey == make_key(KeyIndexList, Record) of
        false ->
            lookup_private(PrivateKey, KeyIndexList, T);
        _ ->
            H
    end.


%% @doc 存储数据, 必须传入新旧record, 因为exia不额外存储 key -> record</br>
%% 当旧record不存在时传undefined
-spec store(tuple(), tuple(), #exia{}) -> #exia{}.
store(OldRecord, NewRecord, Exia) ->
    #exia{
        private_key = PrivateKey,
        dict = DictElementList,
        tree = TreeElementList
    } = Exia,
    %% 存储dict
    DictElementList1 =
        lists:map(fun(#exia_i{key = Key, index = Index} = Element) ->
            OldIndexElement = make_index_element(PrivateKey, Key, OldRecord),
            NewIndexElement = make_index_element(PrivateKey, Key, NewRecord),
            Index1 = exia_dict:store(OldIndexElement, NewIndexElement, Index),
            Element#exia_i{index = Index1}
                  end, DictElementList),
    %% 存储tree
    TreeElementList1 =
        lists:map(fun(#exia_i{key = Key, index = Index} = Element) ->
            OldIndexElement = make_index_element(PrivateKey, Key, OldRecord),
            NewIndexElement = make_index_element(PrivateKey, Key, NewRecord),
            Index1 = exia_tree:store(OldIndexElement, NewIndexElement, Index),
            Element#exia_i{index = Index1}
                  end, TreeElementList),
    Exia#exia{dict = DictElementList1, tree = TreeElementList1}.


%% @doc 删除数据
-spec erase(integer(), #exia{}) -> #exia{}.
erase(Exia, Record) ->
    #exia{
        private_key = PrivateKey,
        dict = DictElementList,
        tree = TreeElementList
    } = Exia,
    %% 删除dict
    DictElementList1 =
        lists:map(fun(#exia_i{key = Key, index = Index} = Element) ->
            IndexElement = make_index_element(PrivateKey, Key, Record),
            Index1 = exia_dict:erase(IndexElement, Index),
            Element#exia_i{index = Index1}
                  end, DictElementList),
    %% 删除tree
    TreeElementList1 =
        lists:map(fun(#exia_i{key = Key, index = Index} = Element) ->
            IndexElement = make_index_element(PrivateKey, Key, Record),
            Index1 = exia_tree:erase(IndexElement, Index),
            Element#exia_i{index = Index1}
                  end, TreeElementList),
    Exia#exia{dict = DictElementList1, tree = TreeElementList1}.


%% @doc 根据索引遍历数据</br>
%% tree索引遍历由key的值从小到大</br>
%% dict索引随机顺序
-spec fold(integer(), element_key(), Fun, Acc :: term(), #exia{}) -> Acc1 :: term()
    when Fun :: fun((#exia_i{}, Acc :: term()) -> Acc1 :: term()).
fold(ExiaRecordPos, ElementKey, Fun, Acc, Exia) when is_function(Fun, 2) ->
    ElementList = element(ExiaRecordPos, Exia),
    case get_element(ElementKey, ElementList) of
        false ->
            erlang:error(no_index);
        #exia_i{index = Index} ->
            fold_1(ExiaRecordPos, Fun, Acc, Index)
    end.

fold_1(#exia.tree, Fun, Acc, Index) ->
    exia_tree:fold(Fun, Acc, Index);
fold_1(#exia.dict, Fun, Acc, Index) ->
    exia_dict:fold(Fun, Acc, Index);
fold_1(_, _, _, _) ->
    erlang:error(badarg).


%% @doc 根据索引和key值范围遍历数据, max >= key >= min</br>
%% tree索引遍历由key的值从小到大</br>
%% dict索引随机顺序
-spec fold_by_range(integer(), element_key(), Fun, Acc :: term(), #exia{}, term(), term()) -> Acc1 :: term()
    when Fun :: fun((#exia_i{}, Acc :: term()) -> Acc1 :: term()).
fold_by_range(ExiaRecordPos, ElementKey, Fun, Acc, Exia, Max, Min) when is_function(Fun, 2), Max >= Min ->
    ElementList = element(ExiaRecordPos, Exia),
    case get_element(ElementKey, ElementList) of
        false ->
            erlang:error(no_index);
        #exia_i{index = Index} ->
            fold_by_range_1(ExiaRecordPos, Fun, Acc, Index, Max, Min)
    end.

fold_by_range_1(#exia.tree, Fun, Acc, Index, Max, Min) ->
    exia_tree:fold_by_range(Fun, Acc, Index, Max, Min);
fold_by_range_1(#exia.dict, Fun, Acc, Index, Max, Min) ->
    exia_dict:fold_by_range(Fun, Acc, Index, Max, Min);
fold_by_range_1(_, _, _, _, _, _) ->
    erlang:error(badarg).

make_index_element(_PrivateKey, _Key, undefined) ->
    undefined;
make_index_element(PrivateKey, Key, Record) ->
    #exia_ie{
        private_key = make_key(PrivateKey, Record),
        key = make_key(Key, Record),
        record = Record
    }.

make_key(KeyIndex, Record) when is_integer(KeyIndex) ->
    element(KeyIndex, Record);
make_key(KeyIndexList, Record) when is_list(KeyIndexList) ->
    [element(Pos, Record) || Pos <- KeyIndexList].

-spec get_element(key()|alias(), [#exia_i{}]) -> false |#exia_i{}.
get_element(Key, ElementList) when is_list(Key) orelse is_integer(Key) ->
    lists:keyfind(Key, #exia_i.key, ElementList);
get_element(Alias, ElementList) when is_atom(Alias) ->
    lists:keyfind(Alias, #exia_i.alias, ElementList).

-compile({nowarn_unused_function, {set_element, 3}}).
-spec set_element(key()|alias(), [#exia_i{}], #exia_i{}) -> [#exia_i{}].
set_element(Key, ElementList, Element) when is_list(Key) orelse is_integer(Key) ->
    lists:keystore(Key, #exia_i.key, ElementList, Element);
set_element(Alias, ElementList, Element) when is_atom(Alias) ->
    lists:keystore(Alias, #exia_i.alias, ElementList, Element).

-spec delete_element(key()|alias(), [#exia_i{}]) -> [#exia_i{}].
delete_element(Key, ElementList) when is_list(Key) orelse is_integer(Key) ->
    lists:keydelete(Key, #exia_i.key, ElementList);
delete_element(Alias, ElementList) when is_atom(Alias) ->
    lists:keydelete(Alias, #exia_i.alias, ElementList).