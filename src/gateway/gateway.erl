%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 网关
%%% @end
%%%-------------------------------------------------------------------
-module(gateway).
-author("dominic").

-behaviour(ranch_protocol).

-include("util.hrl").
-include("gateway.hrl").

%% API
-export([start_link/3, init/1]).

start_link(Ref, ranch_tcp, _Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref]),
    {ok, Pid}.

init([Ref]) ->
    {ok, Socket} = ranch:handshake(Ref),
    loop(#gateway{socket = Socket}).


loop(#gateway{socket = Socket, bin = OldBin} = State) ->
    case ranch_tcp:recv(Socket, 0, 60000) of
        {ok, Data} ->
            case <<Data/binary, OldBin/binary>> of
                <<Len:?GATEWAY_LEN, Proto:?GATEWAY_PROTO, ProtoBin:Len, Remain/binary>> ->
                    try
                        case proto_mapping:decode(Proto, ProtoBin) of
                            {error, Error} ->
                                ?LOG_ERROR("~w", [Error]),
                                close(State);
                            Msg ->
                                State1 = State#gateway{bin = Remain},
                                State2 = handle_msg(Msg, State1),
                                loop(State2)
                        end
                    catch
                        C:E:S ->
                            ?LOG_ERROR("~w, ~w~n~w", [C, E, S]),
                            close(State)
                    end;
                Bin ->
                    loop(State#gateway{bin = Bin})
            end;
        _ ->
            close(State)
    end.

handle_msg(Msg, #gateway{player = Player} = State) when is_pid(Player) ->
    exia:cast(Player, {gateway_msg, Msg}),
    State.

close(#gateway{socket = Socket, player = Player} = State) ->
    ok = ranch_tcp:close(Socket),
    ?DO_IF(is_pid(Player), exia:cast(Player, gateway_disconnect)),
    State.
