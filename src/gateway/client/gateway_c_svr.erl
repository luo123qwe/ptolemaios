%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 客户端进程
%%%
%%% 链接到服务器, 模拟收发协议
%%%
%%% 使用gateway_c_sup开启!!
%%% @end
%%%-------------------------------------------------------------------
-module(gateway_c_svr).
-author("dominic").

-behaviour(plm_svr).

-include("plm_lib.hrl").
-include("gateway.hrl").
-include("gateway_12_pb.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([start_link/4, send_proto/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term()} | ignore.
%% 处理服务器信息
-callback handle_msg(Proto :: gateway:proto(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.

%% @private
start_link(Ip, Port, Module, Args) ->
    plm_svr:start_link(?MODULE, [Ip, Port, Module, Args], []).

%% @doc 发送一条协议到服务器
-spec send_proto(pid(), gateway:proto()) -> any().
send_proto(Pid, Msg) ->
    plm_svr:cast(Pid, ?MSG12_SEND_MSG1(Msg)).

%% @private
init([Ip, Port, Module, Args]) ->
    {ok, Socket} = gen_tcp:connect(Ip, Port, [{packet, raw}, binary, {active, true}]),
    plm_svr:put(?PD12_C_SOCKET, Socket),
    plm_svr:put(?PD12_C_BIN, <<>>),
    plm_svr:put(?PD12_C_MODULE, Module),
    ?DYM12_C_CB3(Module, init, [Args]).

%% @private
handle_call(Request, From, State) ->
    Module = plm_svr:get(?PD12_C_MODULE),
    ?DYM12_C_CB3(Module, handle_call, [Request, From, State]).

%% @private
handle_cast(?MSG12_SEND_MSG1(Msg), State) ->
    Bin = gateway:pack(Msg),
    ranch_tcp:send(plm_svr:get(?PD12_C_SOCKET), Bin),
    {noreply, State};
handle_cast(Request, State) ->
    Module = plm_svr:get(?PD12_C_MODULE),
    ?DYM12_C_CB3(Module, handle_cast, [Request, State]).


%% @private
handle_info({tcp, _Socket, Data}, State) ->
    Data1 = <<Data/binary, (plm_svr:get(?PD12_C_BIN))/binary>>,
    case gateway:unpack(Data1) of
        more ->
            plm_svr:put(?PD12_C_BIN, Data1);
        {ok, Msg, Remain} ->
            Module = plm_svr:get(?PD12_C_MODULE),
            plm_svr:put(?PD12_C_BIN, Remain),
            ?DYM12_C_CB3(Module, handle_msg, [Msg, State]);
        {error, _Error} ->
            {noreply, State}
    end;
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    Module = plm_svr:get(?PD12_C_MODULE),
    ?DYM12_C_CB3(Module, handle_info, [Info, State]).