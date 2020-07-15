%%%-------------------------------------------------------------------
%%% @author dominic
%%% @doc
%%% 键值型数据结构操作
%%% 效率可能损耗多一点, 应该也不会差很多
%%% 主要解决深嵌套时的查询和更改
%%% @end
%%%-------------------------------------------------------------------
-module(kv_op).
-author("dominic").

%% API
-export([lookup/3]).

%% 从dict.erl复制过来, erl版本变化时可能会有问题, ?PTOLEMAIOS_MASK_COPY_FROM_ERL
-record(dict, {size, n, maxn, bso, exp_size, con_size, empty, segs}).
%% 如果使用list作为key, 外面必须再套一层[]
%% 特别支持key+pos(record)
-type key() :: [term()]|term()|{Key :: term(), Pos :: non_neg_integer()}.
-type value() :: term().
%% ets仅支持set和ordered_set, 不要(正常也不会)定义dict的record
-type struct() :: [{key(), value()}|tuple()]|tuple()|map()|dict:dict()|ets:tab().

%% @doc 查找一个value

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
lookup([H | T], Struct, _Default) when is_tuple(Struct) ->
    element(H, Struct);
lookup([H | T], Struct, Default) when is_map(Struct) ->
    maps:get(H, Struct, Default);
lookup([H | T], Struct, Default) when is_atom(Struct);is_reference(Struct) ->
    case ets:lookup(Struct, H) of
        [Value] ->
            lookup(T, Value, Default);
        _ -> Default
    end;
lookup(Key, Struct, Default) ->
    lookup([Key], Struct, Default).

%% 因为仅有叶节点有默认值= =
%% 所以仅当叶节点前的数据都存在, 才储存
store_in_leaf([{Key, Pos} | T], Value, Struct) ->
    case T of
        [] ->
            lists:keystore(Key, Pos, Struct, Value);
        _ ->
            case lists:keyfind(Key, Pos, Struct) of
                false -> Struct;
                SValue ->
                    SValue1 = store_in_leaf(T, Value, SValue),
                    lists:keyreplace(Key, Pos, Struct, SValue1)
            end
    end;
store_in_leaf([H | T], Value, Struct) when is_list(Struct) ->
    case T of
        [] ->
            lists:keystore(H, 1, Struct, Value);
        _ ->
            case lists:keyfind(H, 1, Struct) of
                false -> Struct;
                {_, SValue} ->
                    SValue1 = store_in_leaf(T, Value, SValue),
                    lists:keyreplace(H, 1, Struct, {H, SValue1})
            end
    end;
store_in_leaf([H | T], Value, Struct) when is_record(Struct, dict) ->
    case T of
        [] ->
            dict:store(H, Value, Struct);
        _ ->
            case dict:find(H, Struct) of
                error -> Struct;
                {_, SValue} ->
                    SValue1 = store_in_leaf(T, Value, SValue),
                    dict:store(H, SValue1, Struct)
            end
    end;
store_in_leaf([H | T], Value, Struct) when is_tuple(Struct) ->
    case T of
        [] ->
            setelement(H, Struct, Value);
        _ ->
            SValue = element(H, Struct),
            SValue1 = store_in_leaf(T, Value, SValue),
            setelement(H, Struct, SValue1)
    end;
store_in_leaf([H | T], Value, Struct) when is_map(Struct) ->
    case T of
        [] ->
            Struct#{H => Value};
        _ ->
            case Struct of
                #{H := SValue} ->
                    SValue1 = store_in_leaf(T, Value, SValue),
                    Struct#{H => SValue1};
                _ ->
                    Struct
            end
    end;
store_in_leaf([H | T], Value, Struct) when is_atom(Struct);is_reference(Struct) ->
    case T of
        [] ->
            ets:insert(Struct, Value);
        _ ->
            case ets:lookup(Struct, H) of
                [SValue] ->
                    SValue1 = store_in_leaf(T, Value, SValue),
                    ets:insert(Struct, SValue1);
                _ ->
                    Struct
            end
    end;
store_in_leaf(Key, Value, Struct) ->
    store_in_leaf([Key], Value, Struct).

%% store+default
%% store+stack