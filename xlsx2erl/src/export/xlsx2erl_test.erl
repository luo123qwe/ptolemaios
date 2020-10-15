%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc test
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xlsx2erl_test).
-author("dominic").

-behaviour(xlsx2erl_export).

-include("xlsx2erl.hrl").

%%%%%%%%%%%record define start%%%%%%%%%%%%%%%%%%%
-record(theme_info, {a, b, c, d, e, f}).
-record(theme_symbol, {a, b, c, d, e, f, g, h, i, j, k, l, m, n, o}).
%%%%%%%%%%%record define end%%%%%%%%%%%%%%%%%%%%%

%% API
-export([compile/1, clean/1]).

compile(#callback_args{filename = FileName}) ->
    RecordDef = [
        {theme_info, record_info(fields, theme_info)},
        {theme_symbol, record_info(fields, theme_symbol)}
    ],
    SheetList = xlsx2erl:get_sheet_data(RecordDef, FileName),
    io:format("~p~n", [SheetList]).

clean(#callback_args{}) ->
    io:format("clean~n", []).