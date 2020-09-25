%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc exia测试用
%%% @end
%%%-------------------------------------------------------------------
-module(exia_test).

-behaviour(exia).
-include("exia.hrl").

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(exia_test_state, {id}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start() ->
    exia:start(?MODULE, [], []).

init([]) ->
    exia:return({ok, #exia_test_state{}}).


handle_call(_Request, _From, State = #exia_test_state{}) ->
    {reply, ok, State}.

handle_cast(return, State = #exia_test_state{}) ->
    exia:return({noreply, State#exia_test_state{id = return}});
handle_cast(_Request, State = #exia_test_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #exia_test_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #exia_test_state{}) ->
    exia:return(ok).

code_change(_OldVsn, State = #exia_test_state{}, _Extra) ->
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
                    ?_assertEqual(#exia_test_state{}, exia:call_execute(Pid, fun(S) -> {reply, S, S} end)),
                    
                    ?_test(exia:cast_execute(Pid, fun(S) -> {noreply, S#exia_test_state{id = cast_execute}} end)),
                    ?_assertEqual(#exia_test_state{id = cast_execute},
                        exia:call_execute(Pid, fun(S) -> {reply, S, S} end)),
                    
                    ?_test(exia:cast(Pid, return)),
                    ?_assertEqual(#exia_test_state{id = return},
                        exia:call_execute(Pid, fun(S) -> {reply, S, S} end)),
                    
                    ?_test(begin
                               Self = self(),
                               exia:cast_execute(Pid, fun(S) ->
                                   exia:cast_after(1000, Self, cast_after),
                                   {noreply, S#exia_test_state{id = cast_after}} end),
                               receive {'$gen_cast', cast_after} -> ok after 1100 -> throw(timeout) end
                           end),
                    ?_assertEqual(#exia_test_state{id = cast_after},
                        exia:call_execute(Pid, fun(S) -> {reply, S, S} end)),
                    
                    ?_assert(exia:call_execute(Pid, fun(S) ->
                        {reply, exia:get_millisecond() == exia:get_millisecond(), S}
                                                    end)),
                    
                    ?_test(begin
                               Self = self(),
                               exia:cast_execute(Pid, fun(S) ->
                                   exia:cast_at(exia:get_millisecond() + 1000, Self, cast_at),
                                   {noreply, S#exia_test_state{id = cast_at}} end),
                               receive {'$gen_cast', cast_at} -> ok after 1100 -> throw(timeout) end
                           end),
                    ?_assertEqual(#exia_test_state{id = cast_at},
                        exia:call_execute(Pid, fun(S) -> {reply, S, S} end)),
                    
                    ?_test(begin
                               Self = self(),
                               exia:cast_execute(Pid, fun(S) ->
                                   exia:cast_at_immediately(exia:get_millisecond() + 1000, Self, cast_at_immediately),
                                   timer:sleep(2000),
                                   {noreply, S#exia_test_state{id = cast_at_immediately}} end),
                               receive {'$gen_cast', cast_at_immediately} -> ok after 1100 -> throw(timeout) end
                           end),
                    ?_assertEqual(#exia_test_state{id = cast_at_immediately},
                        exia:call_execute(Pid, fun(S) -> {reply, S, S} end)),
                    
                    ?_test(exia:cast_execute(Pid, fun(S) -> exia:eput(exia, exia), throw(test), {noreply, S} end)),
                    ?_assertEqual(undefined, exia:call_execute(Pid, fun(S) -> {reply, exia:eget(exia, undefined), S} end)),
                    ?_test(exia:cast_execute(Pid, fun(S) ->
                        exia:eput(exia, exia),
                        exia:flush(S),
                        throw(test),
                        {noreply, S}
                                                  end)),
                    ?_assertEqual(exia, exia:call_execute(Pid, fun(S) -> {reply, exia:eget(exia, undefined), S} end)),
                    
                    ?_test(exia:stop(Pid))
                ]
            end
        }
    ].

-endif.