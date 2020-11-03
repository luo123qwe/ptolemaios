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

-include("util.hrl").

%% API
-export([start/2, start/3, start_link/2, start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start(Args, Options) ->
    exia:start(?MODULE, Args, Options).

start(Name, Args, Options) ->
    exia:start(Name, ?MODULE, Args, Options).

start_link(Args, Options) ->
    exia:start_link(?MODULE, Args, Options).

start_link(Name, Args, Options) ->
    exia:start_link(Name, ?MODULE, Args, Options).

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