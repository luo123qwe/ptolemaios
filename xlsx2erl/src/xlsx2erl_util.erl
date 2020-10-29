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
-export([
    set_row/1, clean_row/0,
    convert_bin/1, convert_int/1, convert_float/1, convert_term/1, convert_json/1
]).


-export([to_iolist/1]).

%% @doc 二进制文本
-spec convert_bin(non_neg_integer()) -> binary().
convert_bin(Index) ->
    {Line, Field, SheetAtom} = get_field(Index),
    try unicode:characters_to_binary(Field)
    catch
        _:_ ->
            io:format("配错了!!!!!,工作簿 ~p 数据表 ~p 第 ~p 行: ~p~n", [SheetAtom, SheetAtom, Line, Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc 整数
-spec convert_int(non_neg_integer()) -> integer().
convert_int(Index) ->
    {Line, Field, SheetAtom} = get_field(Index),
    try list_to_integer(Field)
    catch
        _:_ ->
            io:format("配错了!!!!!,工作簿 ~p 数据表 ~p 第 ~p 行: ~p~n", [SheetAtom, SheetAtom, Line, Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc 浮点数
-spec convert_float(non_neg_integer()) -> float().
convert_float(Index) ->
    {Line, Field, SheetAtom} = get_field(Index),
    try
        case catch list_to_float(Field) of
            Float when is_float(Float) -> Float;
            _ -> list_to_integer(Field)
        end
    catch
        _:_ ->
            io:format("配错了!!!!!,工作簿 ~p 数据表 ~p 第 ~p 行: ~p~n", [SheetAtom, SheetAtom, Line, Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc json
-spec convert_json(non_neg_integer()) -> jsx:json_term().
convert_json(Index) ->
    {Line, Field, SheetAtom} = get_field(Index),
    try jsx:decode(unicode:characters_to_binary(Field))
    catch
        _:_ ->
            io:format("配错了!!!!!,工作簿 ~p 数据表 ~p 第 ~p 行: ~p~n", [SheetAtom, SheetAtom, Line, Field]),
            exit(?FUNCTION_NAME)
    end.

%% @doc erlang结构
-spec convert_term(non_neg_integer()) -> term().
convert_term(Index) ->
    {Line, Field, SheetAtom} = get_field(Index),
    try eval(Field)
    catch
        _:_ ->
            io:format("配错了!!!!!,工作簿 ~p 数据表 ~p 第 ~p 行: ~p~n", [SheetAtom, SheetAtom, Line, Field]),
            exit(?FUNCTION_NAME)
    end.

get_field(Index) ->
    case get(?PD_XLSX2ERL_ROW) of
        #row{line = Line, record = Record} ->
            case catch element(Index, Record) of
                Field when is_list(Field) ->% string
                    {Line, Field, element(1, Record)};
                _ ->
                    io:format("=============找程序============="),
                    exit(badarg)
            end;
        _ ->
            io:format("=============找程序============="),
            exit(badarg)
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

%% @private
set_row(Row) ->
    put(?PD_XLSX2ERL_ROW, Row).

%% @private
clean_row() ->
    put(?PD_XLSX2ERL_ROW, undefined).

%% @doc 转换成IOList, 文本请传入binary
to_iolist(Binary) when is_binary(Binary) ->
    ["<<\"", unicode:characters_to_list(Binary), "\"/utf8>>"];
to_iolist(Term) ->
    io_lib:format("~w", [Term]).