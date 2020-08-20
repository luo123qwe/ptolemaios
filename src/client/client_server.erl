%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 模拟客户端
%%% @end
%%%-------------------------------------------------------------------
-module(client_server).
-author("dominic").

-behaviour(exia).

-include("util.hrl").
-include("gateway.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([start/0, send_msg/2]).

start() ->
    exia:start(?MODULE, [], []).

send_msg(Pid, Msg) ->
    exia:cast(Pid, {msg, Msg}).

init([]) ->
    {ok, Port} = application:get_env(ptolemaios, port),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{packet, raw}, binary, {active, true}]),
    {ok, #gateway{socket = Socket}}.

handle_call(Request, From, State) ->
    ?LOG_ERROR("~w ~w", [Request, From]),
    {reply, error, State}.

handle_cast({msg, Msg}, #gateway{socket = Socket} = State) ->
    Bin = gateway_server:pack(Msg),
    ranch_tcp:send(Socket, Bin),
    ?LOG_NOTICE("send ~w", [Msg]),
    {noreply, State};
handle_cast(Request, State) ->
    ?LOG_ERROR("~w", [Request]),
    {noreply, State}.


handle_info({tcp, Socket, Data}, #gateway{socket = Socket, bin = OldBin} = State) ->
    case <<Data/binary, OldBin/binary>> of
        <<Len:?GATEWAY_LEN, Proto:?GATEWAY_PROTO, ProtoBin:Len/binary, Remain/binary>> ->
            try
                case proto_mapping:decode(Proto, ProtoBin) of
                    {error, Error} ->
                        ?LOG_ERROR("~w", [Error]),
                        {stop, normal, State};
                    Msg ->
                        State1 = State#gateway{bin = Remain},
                        ?LOG_NOTICE("receive ~w", [Msg]),
                        {noreply, State1}
                end
            catch
                C:E:S ->
                    ?LOG_ERROR("~w, ~w~n~w", [C, E, S]),
                    {stop, normal, State}
            end;
        Bin ->
            {noreply, State#gateway{bin = Bin}}
    end;
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    ?LOG_ERROR("~w", [Info]),
    {noreply, State}.