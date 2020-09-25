%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 路由进程
%%% @end
%%%-------------------------------------------------------------------
-module(gw_svr).
-author("dominic").

-behaviour(exia).

-include("util.hrl").
-include("gw.hrl").
-include("player.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([start_link/3, send_proto/2]).

%% @private
start_link(Ref, Transport, Opts) ->
    exia:start_link(?MODULE, [Ref, Transport, Opts], []).

%% @doc 发送一条协议到网关进程
-spec send_proto(pid(), gw:proto()) -> any().
send_proto(Pid, Msg) ->
    exia:cast(Pid, ?MSG_GW_SEND_MSG1(Msg)).

%% @private
init([Ref, ranch_tcp, []]) ->
    %% ranch需要先返回pid, 再handshake
    exia:send(self(), {handshake, Ref}),
    {ok, undefined}.

%% @private
handle_call(Request, From, State) ->
    ?LOG_ERROR("~w ~w", [Request, From]),
    {reply, error, State}.

%% @private
handle_cast(?MSG_GW_SEND_MSG1(Msg), #gw{socket = Socket} = State) ->
    Bin = gw:pack(Msg),
    ranch_tcp:send(Socket, Bin),
    {noreply, State};
handle_cast(Request, State) ->
    ?LOG_ERROR("~w", [Request]),
    {noreply, State}.

%% @private
handle_info({handshake, Ref}, _) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = ranch_tcp:setopts(Socket, [{active, true}]),
    {noreply, #gw{socket = Socket}};
handle_info({tcp, Socket, Data}, #gw{socket = Socket, bin = OldBin} = State) ->
    case <<Data/binary, OldBin/binary>> of
        <<Len:?GATEWAY_LEN, Proto:?GATEWAY_PROTO, ProtoBin:Len/binary, Remain/binary>> ->
            try
                case proto_mapping:decode(Proto, ProtoBin) of
                    {error, Error} ->
                        ?LOG_ERROR("~w", [Error]),
                        State1 = close(State),
                        {stop, normal, State1};
                    Msg ->
                        State1 = State#gw{bin = Remain},
                        State2 = player_1_handle:handle(Msg, State1),
                        {noreply, State2}
                end
            catch
                C:E:S ->
                    ?LOG_ERROR("~w, ~w~n~w", [C, E, S]),
                    ErrorState = close(State),
                    {stop, normal, ErrorState}
            end;
        Bin ->
            {noreply, State#gw{bin = Bin}}
    end;
handle_info({tcp_closed, _}, State) ->
    State1 = close(State),
    {stop, normal, State1};
handle_info({tcp_error, _, Reason}, State) ->
    State1 = close(State),
    {stop, Reason, State1};
handle_info(Info, State) ->
    ?LOG_ERROR("~w", [Info]),
    {noreply, State}.

close(#gw{socket = Socket, player_pid = Player} = State) ->
    catch ranch_tcp:close(Socket),
    ?DO_IF(is_pid(Player), exia:cast(Player, ?MSG_PLAYER_GW_DISCONNECT)),
    State.