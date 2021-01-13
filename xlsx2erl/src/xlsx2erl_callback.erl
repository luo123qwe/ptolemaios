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

-callback update_dets(#xlsx2erl_sheet{}, [#xlsx2erl_raw_row{}]) -> any().
-callback compile(#xlsx2erl_cb_args{}) -> any().
-callback clean(#xlsx2erl_cb_args{}) -> any().
