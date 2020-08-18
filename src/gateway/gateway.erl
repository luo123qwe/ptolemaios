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
-include("player_1_pb.hrl").

%% API
-export([start_link/3, init/1, pack/1]).

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
                                State2 = player_1_handle:handle(Msg, State1),
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

close(#gateway{socket = Socket, player = Player} = State) ->
    ok = ranch_tcp:close(Socket),
    ?DO_IF(is_pid(Player), exia:cast(Player, gateway_disconnect)),
    State.

pack(Msg) ->
    case proto_mapping:encode(Msg) of
        Bin when is_binary(Bin) ->
            Proto = proto_mapping:proto(Msg),
            Len = byte_size(Bin),
            <<Len:?GATEWAY_LEN, Proto:?GATEWAY_PROTO, Bin/binary>>;
        Error ->
            throw(Error)
    end.