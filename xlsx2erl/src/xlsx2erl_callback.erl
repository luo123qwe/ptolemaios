%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc behaviour
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(xlsx2erl_callback).
-author("dominic").

-include("xlsx2erl.hrl").

-callback update_dets(FileName :: file:filename()) -> any().
-callback compile(#callback_args{}) -> any().
-callback clean(#callback_args{}) -> any().
