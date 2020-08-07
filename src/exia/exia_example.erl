%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(exia_example).

-behaviour(gen_server).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(exia_example_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start() ->
    exia:start({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    throw({ok, #exia_example_state{}}).

handle_call(_Request, _From, State = #exia_example_state{}) ->
    throw({reply, ok, State}).

handle_cast(_Request, State = #exia_example_state{}) ->
    throw({noreply, State}).

handle_info(_Info, State = #exia_example_state{}) ->
    throw({noreply, State}).

terminate(_Reason, _State = #exia_example_state{}) ->
    ok.

code_change(_OldVsn, State = #exia_example_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
