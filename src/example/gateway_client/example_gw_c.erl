%%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 客户端例子
%%% 打印收到的协议
%%% @end
%%%-------------------------------------------------------------------
-module(example_gw_c).
-author("dominic").

-behaviour(gw_c_svr).

-include("util.hrl").
-include("gw.hrl").

%% API
-export([handle_msg/2, init/1, handle_call/3, handle_cast/2, handle_info/2]).


handle_msg(Msg, State) ->
    ?LOG_NOTICE("client receive ~p", [Msg]),
    {noreply, State}.

init(_Args) ->
    {ok, undefined}.

handle_call(Request, From, State) ->
    ?LOG_ERROR("unknow call from ~p~n~p", [Request, From]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOG_ERROR("unknow cast~n~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG_ERROR("unknow info~n~p", [Info]),
    {noreply, State}.