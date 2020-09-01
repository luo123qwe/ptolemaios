%% @hidden
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 模板
%%% @end
%%%-------------------------------------------------------------------
-module(exia_template).
-author("dominic").

-behaviour(exia).

-include("util.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([start/0]).

start() ->
    exia:start(?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call(Request, From, State) ->
    ?LOG_ERROR("~w ~w", [Request, From]),
    {reply, error, State}.

handle_cast(Request, State) ->
    ?LOG_ERROR("~w", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG_ERROR("~w", [Info]),
    {noreply, State}.