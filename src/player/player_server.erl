%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 玩家进程
%%% @end
%%%-------------------------------------------------------------------
-module(player_server).
-author("dominic").

-behaviour(exia).

-include("util.hrl").
-include("player.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([start_link/1]).

start_link(Id) ->
    exia:start_link(name(Id), ?MODULE, [Id], []).

name(Id) ->
    atom_to_list("player_" ++ integer_to_list(Id)).

init([Id]) ->
    {ok, #player{id = Id}}.

handle_call(Request, From, State) ->
    ?LOG_ERROR("~w ~w", [Request, From]),
    {reply, error, State}.

handle_cast({gateway, Msg}, State) ->
    State1 = proto_mapping:route(Msg, State),
    {noreply, State1};
handle_cast(Request, State) ->
    ?LOG_ERROR("~w", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG_ERROR("~w", [Info]),
    {noreply, State}.