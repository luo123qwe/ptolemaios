%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 模板
%%% @end
%%% Created : 27. 5月 2020 16:04
%%%-------------------------------------------------------------------
-module(exia_template).
-author("dominic").

%% API
-export([]).

-compile(inline).

-type key() :: list().% [record_index]
-type alias() :: atom().

-include("exia.hrl").


new(Type, KeyIndexList, Alias) ->
    new(Type, KeyIndexList, Alias, #exia_index{}).

new(TypePos, KeyIndexList, Alias, ExiaIndex) when is_list(KeyIndexList), is_atom(Alias) ->
    ElementList = element(TypePos, ExiaIndex),
    case
        lists:keymember(KeyIndexList, #exia_element.key, ElementList)
            orelse lists:keymember(Alias, #exia_element.alias, ElementList)
    of
        true ->% user 是否抛错
            ExiaIndex;
        false ->
            NewElement = #exia_element{key = KeyIndexList, alias = Alias, index = new_index(TypePos)},
            setelement(TypePos, ExiaIndex, [NewElement | ElementList])
    end.

new_index(#exia_index.tree) ->
    exia_tree:new();
new_index(#exia_index.dict) ->
    dict:new();
new_index(_) ->
    erlang:error(badarg).

lookup(TypePos, ElementKey, Key, ExiaIndex) ->
    ElementList = element(TypePos, ExiaIndex),
    case get_index_element(ElementKey, ElementList) of
        false ->
            [];
        #exia_element{index = Index} ->
            lookup_1(TypePos, Key, Index)
    end.

lookup_1(#exia_index.tree, Key, Tree) ->
    exia_tree:lookup(Key, Tree);
lookup_1(#exia_index.dict, Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, RecordList} -> RecordList;
        _ -> []
    end;
lookup_1(_, _, _) ->
    erlang:error(badarg).

lookup_by_private(TypePos, ElementKey, Key, PrivateKey, ExiaIndex) ->
    lookup_by_private_1(PrivateKey, ExiaIndex#exia_index.private_key, lookup(TypePos, ElementKey, Key, ExiaIndex)).

lookup_by_private_1(_PrivateKey, _KeyIndexList, []) ->
    undefined;
lookup_by_private_1(PrivateKey, KeyIndexList, [H | T]) ->
    case PrivateKey == make_key(KeyIndexList, H) of
        false ->
            lookup_by_private_1(PrivateKey, KeyIndexList, T);
        _ ->
            H
    end.

store(#exia_index.dict, IndexElementKey, Record, ExiaIndex) ->
    case get_index_element(IndexElementKey, ExiaIndex#exia_index.dict) of
        false ->
            erlang:error(no_index);
        #exia_element{key = KeyIndexList, index = Dict} ->
            dict:store(make_key(KeyIndexList, Record), Record, Dict)
    end;
store(#exia_index.tree, IndexElementKey, Record, ExiaIndex) ->
    case get_index_element(IndexElementKey, ExiaIndex#exia_index.dict) of
        false ->
            erlang:error(no_index);
        #exia_element{key = KeyIndexList, index = Tree} ->
            exia_tree:store(make_key(KeyIndexList, Record), Record, Tree)
    end.

delete() ->
.

fold() ->
.

fold_by_range() ->
.

make_key(KeyIndex, Record) when is_integer(KeyIndex) ->
    element(KeyIndex, Record);
make_key(KeyIndexList, Record) when is_list(KeyIndexList) ->
    [element(Pos, Record) || Pos <- KeyIndexList].

-spec get_index_element(key()|alias(), [#exia_element{}]) -> false |#exia_element{}.
get_index_element(KeyIndexList, ElementList) ->
    lists:keyfind(KeyIndexList, #exia_element.key, ElementList);
get_index_element(Alias, ElementList) ->
    lists:keyfind(Alias, #exia_element.alias, ElementList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% user code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_exist() ->
    erlang:error(not_implemented).