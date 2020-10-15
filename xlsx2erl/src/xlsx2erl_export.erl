%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc behaviour
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xlsx2erl_export).
-author("dominic").

-include("xlsx2erl.hrl").

-callback compile(#callback_args{}) -> any().
-callback clean(#callback_args{}) -> any().
