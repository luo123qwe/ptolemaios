%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 路由进程
%%% @end
%%%-------------------------------------------------------------------
-module(gateway_svr).
-author("dominic").

-behaviour(plm_svr).

-include("plm_lib.hrl").
-include("game.hrl").
-include("game_11_pb.hrl").
-include("gateway.hrl").
-include("player.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start_link/3, send_proto/2]).

%% @private
start_link(Ref, Transport, Opts) ->
    plm_svr:start_link(?MODULE, [Ref, Transport, Opts], []).

%% @doc 发送一条协议到网关进程
-spec send_proto(pid(), gateway:proto()) -> any().
send_proto(Pid, Msg) ->
    plm_svr:cast(Pid, ?MSG12_SEND_MSG1(Msg)).

%% @private
init([Ref, ranch_tcp, []]) ->
    %% ranch需要先返回pid, 再handshake
    plm_svr:send(self(), {handshake, Ref}),
    {ok, undefined}.

%% @private
handle_call(Request, From, State) ->
    ?LOG_ERROR("~w ~w", [Request, From]),
    {reply, error, State}.

%% @private
handle_cast(?MSG12_SEND_MSG1(Msg), #gateway_state{socket = Socket} = State) ->
    Bin = gateway:pack(Msg),
    ranch_tcp:send(Socket, Bin),
    {noreply, State};
handle_cast(Request, State) ->
    ?LOG_ERROR("~w", [Request]),
    {noreply, State}.

%% @private
handle_info({handshake, Ref}, _) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = ranch_tcp:setopts(Socket, [{active, true}]),
    {noreply, #gateway_state{socket = Socket}};
handle_info({tcp, Socket, Data}, #gateway_state{socket = Socket, bin = OldBin} = State) ->
    decode_bin(State#gateway_state{bin = <<Data/binary, OldBin/binary>>});
handle_info({tcp_closed, _}, State) ->
    State1 = close(State),
    {stop, normal, State1};
handle_info({tcp_error, _, Reason}, State) ->
    State1 = close(State),
    {stop, Reason, State1};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #gateway_state{player_pid = Pid} = State) ->
    State1 = close(State),
    {stop, normal, State1};
handle_info(Info, State) ->
    ?LOG_ERROR("~w", [Info]),
    {noreply, State}.

terminate(_, State) ->
    case State#gateway_state.account of
        undefined -> skip;
        Account -> plm_ll:release(?M13_LL_ACCOUNT1(Account))
    end,
    case State#gateway_state.player_id of
        undefined -> skip;
        PlayerId ->
            plm_ll:release(?M13_LL_PLAYER_ID1(PlayerId)),
            plm_svr:cast(State#gateway_state.player_pid, ?MSG13_GATEWAY_DISCONNECT)
    end,
    ok.

decode_bin(#gateway_state{bin = Bin, socket = Socket, player_pid = PlayerPid} = State) ->
    case Bin of
        <<Len:?M12_PROTO_LEN, Proto:?M12_PROTO_NUM, ProtoBin:Len/binary, Remain/binary>> ->
            ProtoHead = ?M12_PROTO_HEAD1(Proto),
            State1 = State#gateway_state{bin = Remain},
            try
                case proto_mapping:decode(Proto, ProtoBin) of
                    {error, Error} ->
                        ?LOG_ERROR("~w", [Error]),
                        State2 = close(State1),
                        {stop, normal, State2};
                    Msg when ProtoHead == ?M12 ->% gateway的协议
                        %% 在本进程返回客户端, 省一个?MSG_GATEWAY_SEND_MSG1
                        case catch gateway_12_handle:handle(Msg, State1) of
                            ?M11_EC1(ErrorCode) ->
                                CBin = gateway:pack(#game_s_error{proto = Proto, code = ErrorCode}),
                                ranch_tcp:send(Socket, CBin),
                                decode_bin(State1);
                            {MsgList, #gateway_state{} = State2} ->
                                lists:foreach(fun(CMsg) ->
                                    CMsg1 = ?MATCH(CMsg, #game_s_error{}, CMsg#game_s_error{proto = Proto}, CMsg),
                                    CBin = gateway:pack(CMsg1),
                                    ranch_tcp:send(Socket, CBin)
                                              end, MsgList),
                                decode_bin(State2);
                            Error ->
                                ?LOG_ERROR("~w", [Error]),
                                State2 = close(State1),
                                {stop, normal, State2}
                        end;
                    Msg when is_pid(PlayerPid) ->% 玩家进程的协议
                        plm_svr:cast(PlayerPid, ?MSG13_GATEWAY_PROTO1(Msg)),
                        decode_bin(State1);
                    _ ->
                        CBin = gateway:pack(#game_s_error{proto = Proto, code = ?M11_EC_NOT_LOGIN}),
                        ranch_tcp:send(Socket, CBin),
                        decode_bin(State1)
                end
            catch
                C:E:S ->
                    ?LOG_ERROR("~w, ~w~n~w", [C, E, S]),
                    ErrorState = close(State),
                    {stop, normal, ErrorState}
            end;
        _ ->
            {noreply, State#gateway_state{bin = Bin}}
    end.


close(#gateway_state{socket = Socket} = State) ->
    catch ranch_tcp:close(Socket),
    State.