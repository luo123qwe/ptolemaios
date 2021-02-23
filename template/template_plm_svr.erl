%%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc plm_svr模板
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(template_plm_svr).
-author("dominic").

-behaviour(plm_svr).

-include("plm_lib.hrl").

%% API
-export([start/1, start_link/1, call/2, cast/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start(Args) ->
    plm_svr:start(?MODULE, Args, []).

start_link(Args) ->
    plm_svr:start_link(?MODULE, Args, []).

call(Name, Request) ->
    plm_svr:call(Name, Request).

cast(Name, Request) ->
    plm_svr:cast(Name, Request).

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