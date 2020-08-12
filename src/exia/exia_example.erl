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
    exia:start(?MODULE, [], []).

init([]) ->
    {ok, #exia_example_state{}}.

handle_call(_Request, _From, State = #exia_example_state{}) ->
    {reply, ok, State}.

handle_cast(test_msg_time, State = #exia_example_state{}) ->
    io:format("exia ~w~n", [test_msg_time]),
    io:format("exia ~w~n", [exia:get_msg_second()]),
    timer:sleep(1000),
    io:format("exia ~w~n", [exia:get_msg_second()]),
    exia:send(test_msg_time),
    {noreply, State};
handle_cast(test_expect_time, State = #exia_example_state{}) ->
    io:format("exia ~w~n", [test_expect_time]),
    io:format("exia ~w~n", [exia:get_expect_second()]),
    io:format("exia ~w~n", [exia:get_msg_second()]),
    exia:send(test_expect_time),
    {noreply, State};
handle_cast(test_auto_send, State = #exia_example_state{}) ->
    io:format("exia ~w~n", [test_auto_send]),
    exia:send(test_auto_send),
    {noreply, State};
handle_cast(test_auto_rollback, State = #exia_example_state{}) ->
    io:format("exia ~w~n", [test_auto_rollback]),
    exia:send(test_auto_rollback),
    throw(test),
    {noreply, State};
handle_cast(_Request, State = #exia_example_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #exia_example_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #exia_example_state{}) ->
    ok.

code_change(_OldVsn, State = #exia_example_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================