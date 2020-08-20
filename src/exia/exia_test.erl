%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(exia_test).

-behaviour(exia).
-include("exia.hrl").

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(exia_example_state, {id}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start() ->
    exia:start(?MODULE, [], []).

init([]) ->
    exia:return({ok, #exia_example_state{}}).


handle_call(test_msg_time, _From, State = #exia_example_state{}) ->
    timer:sleep(1000),
    {reply, exia:get_msg_second(), State};
handle_call(test_expect_time, _From, State = #exia_example_state{}) ->
    {reply, exia:get_expect_second(), State};
handle_call(test_auto_send, _From, State = #exia_example_state{}) ->
    exia:send(test_auto_send),
    {reply, ok, State};
handle_call(test_auto_rollback, _From, State = #exia_example_state{}) ->
    exia:send(test_auto_rollback),
    throw(test),
    {reply, ok, State#exia_example_state{id = test_auto_rollback}};
handle_call(test_flush, _From, State = #exia_example_state{}) ->
    exia:send(test_flush),
    exia:flush(State#exia_example_state{id = test_flush}),
    throw(test),
    {reply, ok, State};
handle_call(test_call_return, _From, State = #exia_example_state{}) ->
    exia:return({reply, ok, State#exia_example_state{id = test_call_return}});
handle_call(get_state, _From, State = #exia_example_state{}) ->
    {reply, State, State};
handle_call(_Request, _From, State = #exia_example_state{}) ->
    {reply, ok, State}.

handle_cast(test_cast_return, State = #exia_example_state{}) ->
    exia:return({noreply, State#exia_example_state{id = test_cast_return}});
handle_cast(_Request, State = #exia_example_state{}) ->
    {noreply, State}.

handle_info(test_info_return, State = #exia_example_state{}) ->
    exia:return({noreply, State#exia_example_state{id = test_info_return}});
handle_info(_Info, State = #exia_example_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #exia_example_state{}) ->
    exia:return(ok).

code_change(_OldVsn, State = #exia_example_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

base_test_() ->
    [
        {setup,
            fun() -> {ok, Pid} = exia_test:start(), Pid end,
            fun(Pid) -> Pid end,
            fun(Pid) ->
                [
                    ?_assertEqual(undefined, exia:set_dest(Pid, self())),
                    ?_assertEqual(erlang:system_time(second), exia:warp_call(Pid, test_msg_time)),
                    ?_assertEqual(123, exia:warp_call(Pid, 123123, test_expect_time)),
                    ?_assertEqual(ok, exia:warp_call(Pid, test_auto_send)),
                    ?_test(receive
                               #exia_msg{msg = test_auto_send} -> ok;
                               Other -> throw({test_auto_send, Other})
                           after 0 -> throw(test_auto_send) end),
                    %% 时间太长会默认cancel, 超时得手动设一个短的
                    ?_assertExit({timeout, {exia, call, [Pid, test_auto_rollback, 1000]}}, exia:warp_call(Pid, erlang:system_time(millisecond), test_auto_rollback, 1000)),
                    ?_test(receive
                               #exia_msg{msg = test_auto_rollback} -> throw(test_auto_rollback);
                               Other -> throw({test_auto_rollback, Other})
                           after 0 -> ok end),
                    ?_assertEqual(#exia_example_state{id = undefined}, exia:call(Pid, get_state)),
                    ?_assertExit({timeout, {exia, call, [Pid, test_flush, 1000]}}, exia:warp_call(Pid, erlang:system_time(millisecond), test_flush, 1000)),
                    ?_test(receive
                               #exia_msg{msg = test_flush} -> ok;
                               Other -> throw({test_flush, Other})
                           after 0 -> throw(test_flush) end),
                    ?_assertEqual(#exia_example_state{id = test_flush}, exia:call(Pid, get_state)),
                    ?_test(exia:call(Pid, test_call_return)),
                    ?_assertEqual(#exia_example_state{id = test_call_return}, exia:call(Pid, get_state)),
                    ?_test(exia:cast(Pid, test_cast_return)),
                    ?_assertEqual(#exia_example_state{id = test_cast_return}, exia:call(Pid, get_state)),
                    ?_test(Pid ! test_info_return),
                    ?_assertEqual(#exia_example_state{id = test_info_return}, exia:call(Pid, get_state)),
                    ?_test(exia:stop(Pid))
                ]
            end
        }
    ].

-endif.