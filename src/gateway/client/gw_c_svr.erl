%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 客户端进程
%%%
%%% 链接到服务器, 模拟收发协议
%%%
%%% 使用gw_c_sup开启!!
%%% @end
%%%-------------------------------------------------------------------
-module(gw_c_svr).
-author("dominic").

-behaviour(exia).

-include("util.hrl").
-include("gw.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([start_link/4, send_proto/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term()} | ignore.
%% 处理服务器信息
-callback handle_msg(Proto :: gw:proto(), State :: term()) ->
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
    exia:start_link(?MODULE, [Ip, Port, Module, Args], []).

%% @doc 发送一条协议到服务器
-spec send_proto(pid(), gw:proto()) -> any().
send_proto(Pid, Msg) ->
    exia:cast(Pid, ?MSG_GW_SEND_MSG1(Msg)).

%% @private
init([Ip, Port, Module, Args]) ->
    {ok, Socket} = gen_tcp:connect(Ip, Port, [{packet, raw}, binary, {active, true}]),
    exia:eput(?PD_GW_C_SOCKET, Socket),
    exia:eput(?PD_GW_C_BIN, <<>>),
    exia:eput(?PD_GW_C_MODULE, Module),
    ?DYM_GW_C_CB3(Module, init, [Args]).

%% @private
handle_call(Request, From, State) ->
    Module = exia:eget(?PD_GW_C_MODULE),
    ?DYM_GW_C_CB3(Module, handle_call, [Request, From, State]).

%% @private
handle_cast(?MSG_GW_SEND_MSG1(Msg), State) ->
    Bin = gw:pack(Msg),
    ranch_tcp:send(exia:eget(?PD_GW_C_SOCKET), Bin),
    {noreply, State};
handle_cast(Request, State) ->
    Module = exia:eget(?PD_GW_C_MODULE),
    ?DYM_GW_C_CB3(Module, handle_cast, [Request, State]).


%% @private
handle_info({tcp, _Socket, Data}, State) ->
    Data1 = <<Data/binary, (exia:eget(?PD_GW_C_BIN))/binary>>,
    case gw:unpack(Data1) of
        more ->
            exia:eput(?PD_GW_C_BIN, Data1);
        {ok, Msg, Remain} ->
            Module = exia:eget(?PD_GW_C_MODULE),
            exia:eput(?PD_GW_C_BIN, Remain),
            ?DYM_GW_C_CB3(Module, handle_msg, [Msg, State]);
        {error, _Error} ->
            {noreply, State}
    end;
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    Module = exia:eget(?PD_GW_C_MODULE),
    ?DYM_GW_C_CB3(Module, handle_info, [Info, State]).