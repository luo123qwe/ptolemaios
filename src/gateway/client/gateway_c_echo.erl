%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 打印收到的协议
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gateway_c_echo).
-author("dominic").

-behaviour(gateway_c_svr).

-include("plm_lib.hrl").

%% API
-export([init/1, handle_msg/2, handle_call/3, handle_cast/2, handle_info/2]).

init(_Args) ->
    {ok, undefined}.

handle_msg(Proto, State) ->
    ?LOG_NOTICE("~p",[Proto]),
    {noreply, State}.

handle_call(Request, From, State) ->
    ?LOG_ERROR("unknow call from ~p~n~p", [Request, From]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOG_ERROR("unknow cast~n~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG_ERROR("unknow info~n~p", [Info]),
    {noreply, State}.