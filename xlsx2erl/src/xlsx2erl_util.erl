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
    convert_bin/1, convert_int/1, convert_float/1, convert_term/1
]).


-export([to_iolist/1]).

%% @doc 转换成<<"abcd"/utf8>>
convert_bin(Index) ->
    {Line, Field, SheetAtom} = get_field(Index),
    try unicode:characters_to_binary(Field)
    catch
        _:_ ->
            io:format("bad file ~s in ~p line ~p", [Field, SheetAtom, Line])
    end.

%% @doc 整数
convert_int(Index) ->
    {Line, Field, SheetAtom} = get_field(Index),
    try list_to_integer(Field)
    catch
        _:_ ->
            io:format("bad file ~s in ~p line ~p", [Field, SheetAtom, Line])
    end.

%% @doc 浮点数
convert_float(Index) ->
    {Line, Field, SheetAtom} = get_field(Index),
    try list_to_float(Field)
    catch
        _:_ ->
            io:format("bad file ~s in ~p line ~p", [Field, SheetAtom, Line])
    end.

%% @doc erlang结构
convert_term(Index) ->
    {Line, Field, SheetAtom} = get_field(Index),
    try eval(Field)
    catch
        _:_ ->
            io:format("bad file ~s in ~p line ~p", [Field, SheetAtom, Line])
    end.

get_field(Index) ->
    case get(?PD_XLSX2ERL_ROW) of
        #row{line = Line, record = Record} ->
            case catch element(Index, Record) of
                Field when is_list(Field) ->% string
                    {Line, Field, element(1, Record)};
                _ ->
                    io:format("=============找程序============="),
                    throw(badarg)
            end;
        _ ->
            io:format("=============找程序============="),
            throw(badarg)
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

%% @doc 设置当前行序号, 报错时自动打印
set_row(Row) ->
    put(?PD_XLSX2ERL_ROW, Row).

clean_row() ->
    put(?PD_XLSX2ERL_ROW, undefined).

%% @doc 转换成IOList, 文本请传入binary
to_iolist(Binary) when is_binary(Binary) ->
    ["<<\"", Binary, "\"/utf8>>"];
to_iolist(Term) ->
    io_lib:format("~w", Term).