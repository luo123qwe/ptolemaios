%%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc exia模板
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(template_exia).
-author("dominic").

-behaviour(exia).

-include("ptolemaios_lib.hrl").

%% API
-export([start/1, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start(Args) ->
    exia:start(?MODULE, Args, []).

start_link(Args) ->
    exia:start_link(?MODULE, Args, []).

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