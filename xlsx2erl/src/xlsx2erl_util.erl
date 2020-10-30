%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xlsx2erl_util).
-author("dominic").

-include("xlsx2erl.hrl").

%% 转换文本到erl结构
-export([convert_bin/4, convert_int/4, convert_float/4, convert_term/4, convert_json/4]).


-export([to_iolist/1]).

%% @doc 二进制文本
-spec convert_bin(non_neg_integer(), [atom()], #xlsx2erl_row{}, #xlsx2erl_sheet{}) -> binary().
convert_bin(Index, RecordDef, Row, Sheet) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try unicode:characters_to_binary(Field)
    catch
        _:_ ->
            ?XLSX2ERL_ERROR4(Sheet, Row, "~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc 整数
-spec convert_int(non_neg_integer(), [atom()], #xlsx2erl_row{}, #xlsx2erl_sheet{}) -> integer().
convert_int(Index, RecordDef, Row, Sheet) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try list_to_integer(Field)
    catch
        _:_ ->
            ?XLSX2ERL_ERROR4(Sheet, Row, "~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc 浮点数
-spec convert_float(non_neg_integer(), [atom()], #xlsx2erl_row{}, #xlsx2erl_sheet{}) -> float().
convert_float(Index, RecordDef, Row, Sheet) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try
        case catch list_to_float(Field) of
            Float when is_float(Float) -> Float;
            _ -> list_to_integer(Field)
        end
    catch
        _:_ ->
            ?XLSX2ERL_ERROR4(Sheet, Row, "~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc json
-spec convert_json(non_neg_integer(), [atom()], #xlsx2erl_row{}, #xlsx2erl_sheet{}) -> jsx:json_term().
convert_json(Index, RecordDef, Row, Sheet) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try jsx:decode(unicode:characters_to_binary(Field))
    catch
        _:_ ->
            ?XLSX2ERL_ERROR4(Sheet, Row, "~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc erlang结构
-spec convert_term(non_neg_integer(), [atom()], #xlsx2erl_row{}, #xlsx2erl_sheet{}) -> term().
convert_term(Index, RecordDef, Row, Sheet) ->
    Field = element(Index, Row#xlsx2erl_row.record),
    try eval(Field)
    catch
        _:_ ->
            ?XLSX2ERL_ERROR4(Sheet, Row, "~p: ~p", [lists:nth(Index - 1, RecordDef), Field]),
            exit(?FUNCTION_NAME)
    end.

eval(Str) when is_binary(Str) ->
    eval(binary_to_list(Str));
eval(Str) when is_list(Str) ->
    {ok, Tokens, _} = erl_scan:string(eval_fix_string(Str)),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:exprs(Exprs, []),
    Value.

eval_fix_string([]) ->
    [$.];
eval_fix_string([$.]) ->
    [$.];
eval_fix_string([H | T]) ->
    [H | eval_fix_string(T)].

%% @doc 转换成IOList, 文本请传入binary
to_iolist(Binary) when is_binary(Binary) ->
    %% 文本中含有 " 需要转换成 \"
    ["<<\"", re:replace(unicode:characters_to_list(Binary), "\"", "\\\\\"", [global]), "\"/utf8>>"];
to_iolist(Term) ->
    io_lib:format("~w", [Term]).