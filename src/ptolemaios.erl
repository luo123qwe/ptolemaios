%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 杂项
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ptolemaios).
-author("dominic").

%% API
-export([
    reload/0
]).

%% @doc shell reload代码
reload() ->
    plm_fix_hot:reload_shell(default, [ptolemaios]).
