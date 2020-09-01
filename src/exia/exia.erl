%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @version {@version}
%%% @doc 基于gen_server修改的behaviour.
%%% <dl>
%%% <dt>去掉throw返回的设定</dt>
%%% <dd>```
%%% 用return/1代替, 文档也没说, 看了代码才发现可以这样'''</dd>
%%% <dt>默认消息地址</dt>
%%% <dd>```
%%% 异步消息未指定接收者时, 默认会发到该进程, 需要自行设置
%%% 顺便一提, 消息发送类似cast, 失败也不会返回错误'''</dd>
%%% <dt>消息时间和期望时间</dt>
%%% <dd>```
%%% 消息时间: 消息收到的时间; 期望时间: 消息期望送达时间
%%% 避免erlang内部定时器误差导致的计算错误
%%% 每条信息都会调用一次erlang:system_time(millisecond)
%%% 如果时间使用频率十分低, 可以优化成第一次获取时间时再调用'''</dd>
%%% <dt>模拟进程字典</dt>
%%% <dd>```
%%% 用于回滚, 使用一个map实现'''</dd>
%%% <dt>回滚机制</dt>
%%% <dd>```
%%% 实际业务中, 并不希望进程因为崩溃而退出(重启)
%%% 比起崩溃更加希望它像什么事情都没发生过, 不改变任何数据, 不发送任何消息
%%% 回滚机制有效范围: State, virture数据集, 默认消息地址, 模拟进程字典, 异步消息
%%% 用户可以也可以在一段逻辑成功后flush/1, 类似continue的机制, 分段执行, 失败可以回滚'''</dd>
%%% <dt>exia消息</dt>
%%% <dd>```
%%% #exia_msg{}, 包含期望时间和消息内容
%%% {exia_private, ...}消息为exia内部消息, 用于支持exia进程的功能
%%% 其他消息按照gen_server原有设定解码'''</dd>
%%% </dl>
%%% @end
%%%-------------------------------------------------------------------
-module(exia).
-author("dominic").

-include("util.hrl").
-include("exia.hrl").

%% 原本的API
-export([
    start/3, start/4,
    start_link/3, start_link/4,
    start_monitor/3, start_monitor/4,
    stop/1, stop/3,
    call/2, call/3, cast/2,
    send_request/2, wait_response/2, check_response/2,
    reply/2, abcast/2, abcast/3,
    multi_call/2, multi_call/3, multi_call/4,
    enter_loop/3, enter_loop/4, enter_loop/5, wake_hib/6
]).

%% exia的
-export([
    set_dest/2, get_dest/1,
    call_execute/2, cast_execute/2,
    warp_call/2, warp_call/3, warp_call/4, warp_cast/2, warp_cast/3,
    spawn_call/4, spawn_call/5, spawn_warp_call/4, spawn_warp_call/5, spawn_warp_call/6,
    %% 仅可进程内使用
    set_dest/1, get_dest/0,
    send/1, send/2, send_after/2, send_after/3, send_at/2, send_at/3, send_after/4,
    send_immediately/1, send_immediately/2, send_after_immediately/2, send_after_immediately/3, send_at_immediately/2, send_at_immediately/3, send_after_immediately/4,
    ecast/1, cast_after/2, cast_after/3, cast_at/2, cast_at/3, cast_after/4,
    cast_immediately/1, cast_immediately/2, cast_after_immediately/2, cast_after_immediately/3, cast_at_immediately/2, cast_at_immediately/3, cast_after_immediately/4,
    flush/1, return/1, get_return/1,
    get_expect_millisecond/0, get_expect_second/0, get_msg_millisecond/0, get_msg_second/0,
    eget/2, eset/2
]).

-export([system_continue/3,
    system_terminate/4,
    system_code_change/4,
    system_get_state/1,
    system_replace_state/2,
    format_status/2]).

%% Internal exports
-export([init_it/6]).

-define(
STACKTRACE(),
    element(2, erlang:process_info(self(), current_stacktrace))).

-type server_ref() ::
pid()
| (LocalName :: atom())
| {Name :: atom(), Node :: atom()}
| {'global', GlobalName :: term()}
| {'via', RegMod :: module(), ViaName :: term()}.

-type execute() ::
F :: function()
|{M :: atom(), F :: atom(), A :: list()}
|{M :: atom(), F :: atom()}
|{F :: function(), A :: list()}.

-type request_id() :: term().

%%%=========================================================================
%%%  API
%%%=========================================================================

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term()} | ignore.
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
-callback handle_continue(Info :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
term()),
    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
    Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
-callback format_status(Opt, StatusData) -> Status when
    Opt :: 'normal' | 'terminate',
    StatusData :: [PDict | State],
    PDict :: [{Key :: term(), Value :: term()}],
    State :: term(),
    Status :: term().

-optional_callbacks(
[handle_continue/2, terminate/2, code_change/3, format_status/2]).

%%%  -----------------------------------------------------------------
%%% Starts a generic server.
%%% start(Mod, Args, Options)
%%% start(Name, Mod, Args, Options)
%%% start_link(Mod, Args, Options)
%%% start_link(Name, Mod, Args, Options) where:
%%%    Name ::= {local, atom()} | {global, term()} | {via, atom(), term()}
%%%    Mod  ::= atom(), callback module implementing the 'real' server
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%%    Options ::= [{timeout, Timeout} | {debug, [Flag]}]
%%%      Flag ::= trace | log | {logfile, File} | statistics | debug
%%%          (debug == log && statistics)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% -----------------------------------------------------------------
%% @equiv gen_server:start/3
start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).

%% @equiv gen_server:start/4
start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

%% @equiv gen_server:start_link/3
start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).

%% @equiv gen_server:start_link/4
start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).

%% @equiv gen_server:start_monitor/4
start_monitor(Mod, Args, Options) ->
    gen:start(?MODULE, monitor, Mod, Args, Options).

%% @equiv gen_server:start_monitor/4
start_monitor(Name, Mod, Args, Options) ->
    gen:start(?MODULE, monitor, Name, Mod, Args, Options).


%% -----------------------------------------------------------------
%% Stop a generic server and wait for it to terminate.
%% If the server is located at another node, that node will
%% be monitored.
%% -----------------------------------------------------------------
%% @equiv gen_server:stop/1
stop(Name) ->
    gen:stop(Name).

%% @equiv gen_server:stop/3
stop(Name, Reason, Timeout) ->
    gen:stop(Name, Reason, Timeout).

%% -----------------------------------------------------------------
%% 默认消息地址
%% -----------------------------------------------------------------
%% @doc 设置默认消息地址, 进程内使用
-spec set_dest(server_ref()) -> OldDest :: undefined|server_ref().
set_dest(Dest) ->
    put(?PD_EXIA_DEST, Dest).

%% @doc 设置进程默认消息地址
-spec set_dest(server_ref(), server_ref()) -> OldDest :: undefined|server_ref().
set_dest(Dest, SetDest) ->
    call(Dest, {exia_private, set_dest, SetDest}).

%% @doc 获取默认消息地址, 进程内使用
-spec get_dest() -> undefined|server_ref().
get_dest() ->
    get(?PD_EXIA_DEST).


%% @doc 获取进程默认消息地址
-spec get_dest(server_ref()) -> undefined|server_ref().
get_dest(Dest) ->
    call(Dest, {exia_private, get_dest}).

%% -----------------------------------------------------------------
%% 执行一个函数
%% -----------------------------------------------------------------
%% @doc 执行一个函数, 函数返回的值与handle_call一样
-spec call_execute(server_ref(), execute()) -> term().
call_execute(Dest, Execute) ->
    call(Dest, {exia_private, execute, Execute}).

%% @doc 执行一个函数, 函数返回的值与handle_cast一样
-spec cast_execute(server_ref(), execute()) -> ok.
cast_execute(Dest, Execute) ->
    cast(Dest, {exia_private, execute, Execute}).

%% -----------------------------------------------------------------
%% Make a call to a generic server.
%% If the server is located at another node, that node will
%% be monitored.
%% If the client is trapping exits and is linked server termination
%% is handled here (? Shall we do that here (or rely on timeouts) ?).
%% -----------------------------------------------------------------
%% @equiv gen_server:call/2
call(Name, Request) ->
    case catch gen:call(Name, '$gen_call', Request) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            exit({Reason, {?MODULE, call, [Name, Request]}})
    end.

%% @equiv gen_server:call/3
call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$gen_call', Request, Timeout) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            exit({Reason, {?MODULE, call, [Name, Request, Timeout]}})
    end.

%% -----------------------------------------------------------------
%% 包装成一个exia消息
%% -----------------------------------------------------------------
%% @equiv warp_call(Dest, erlang:system_time(millisecond), Request)
warp_call(Dest, Request) ->
    warp_call(Dest, erlang:system_time(millisecond), Request).

%% @doc 包装成一条exia消息再call
-spec warp_call(server_ref(), integer(), term()) -> term().
warp_call(Dest, ExpectTime, Request) ->
    case catch gen:call(Dest, '$gen_call', #exia_msg{expect_time = ExpectTime, msg = Request}) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            exit({Reason, {?MODULE, call, [Dest, Request]}})
    end.

%% @doc 包装成一条exia消息再call
-spec warp_call(server_ref(), integer(), term(), non_neg_integer()) -> term().
warp_call(Dest, ExpectTime, Request, Timeout) ->
    case catch gen:call(Dest, '$gen_call', #exia_msg{expect_time = ExpectTime, msg = Request}, Timeout) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            exit({Reason, {?MODULE, call, [Dest, Request, Timeout]}})
    end.

%% @equiv warp_cast(Dest, erlang:system_time(millisecond), Request)
warp_cast(Dest, Request) ->
    warp_cast(Dest, erlang:system_time(millisecond), Request).

%% @doc 包装成一条exia消息再cast
-spec warp_cast(server_ref(), non_neg_integer(), term()) -> term().
warp_cast(Dest, ExpectTime, Request) ->
    do_send(Dest, #exia_msg{expect_time = ExpectTime, msg = cast_msg(Request)}).

%% -----------------------------------------------------------------
%% spawn一个进程执行call操作
%% -----------------------------------------------------------------
%% @doc spawn一个进程进行call, 成功或失败时执行对应的函数
-spec spawn_call(server_ref(), term(), undefined|function(), undefined|function()) -> pid().
spawn_call(Dest, Request, SuccessFun, FailFun) ->
    spawn(fun() ->
        case catch gen:call(Dest, '$gen_call', Request) of
            {ok, Res} ->
                case SuccessFun of
                    undefined -> skip;
                    _ -> SuccessFun(Res)
                end;
            {'EXIT', Reason} ->
                case FailFun of
                    undefined -> skip;
                    _ -> FailFun(Reason)
                end
        end
          end).

%% @doc spawn一个进程进行call, 成功或失败时执行对应的函数
-spec spawn_call(server_ref(), term(), non_neg_integer(), undefined|function(), undefined|function()) -> pid().
spawn_call(Dest, Request, Timeout, SuccessFun, FailFun) ->
    spawn(fun() ->
        case catch gen:call(Dest, '$gen_call', Request, Timeout) of
            {ok, Res} ->
                case SuccessFun of
                    undefined -> skip;
                    _ -> SuccessFun(Res)
                end;
            {'EXIT', Reason} ->
                case FailFun of
                    undefined -> skip;
                    _ -> FailFun(Reason)
                end
        end
          end).

%% @equiv spawn_warp_call(Dest, erlang:system_time(millisecond), Request, SuccessFun, FailFun)
spawn_warp_call(Dest, Request, SuccessFun, FailFun) ->
    spawn_warp_call(Dest, erlang:system_time(millisecond), Request, SuccessFun, FailFun).

%% @doc spawn一个进程进行warp_call, 成功或失败时执行对应的函数
%% @see warp_call/3
-spec spawn_warp_call(server_ref(), non_neg_integer(), term(), undefined|function(), undefined|function()) -> pid().
spawn_warp_call(Dest, ExpectTime, Request, SuccessFun, FailFun) ->
    spawn(fun() ->
        case catch gen:call(Dest, '$gen_call', #exia_msg{expect_time = ExpectTime, msg = Request}) of
            {ok, Res} ->
                case SuccessFun of
                    undefined -> skip;
                    _ -> SuccessFun(Res)
                end;
            {'EXIT', Reason} ->
                case FailFun of
                    undefined -> skip;
                    _ -> FailFun(Reason)
                end
        end
          end).


%% @doc spawn一个进程进行warp_call, 成功或失败时执行对应的函数
%% @see warp_call/3
-spec spawn_warp_call(server_ref(), non_neg_integer(), term(), non_neg_integer(), undefined|function(), undefined|function()) -> pid().
spawn_warp_call(Dest, ExpectTime, Request, Timeout, SuccessFun, FailFun) ->
    spawn(fun() ->
        case catch gen:call(Dest, '$gen_call', #exia_msg{expect_time = ExpectTime, msg = Request}, Timeout) of
            {ok, Res} ->
                case SuccessFun of
                    undefined -> skip;
                    _ -> SuccessFun(Res)
                end;
            {'EXIT', Reason} ->
                case FailFun of
                    undefined -> skip;
                    _ -> FailFun(Reason)
                end
        end
          end).


%% -----------------------------------------------------------------
%% Send a request to a generic server and return a Key which should be
%% used with wait_response/2 or check_response/2 to fetch the
%% result of the request.

%% @equiv gen_server:send_request/2
-spec send_request(Name :: server_ref(), Request :: term()) -> request_id().
send_request(Name, Request) ->
    gen:send_request(Name, '$gen_call', Request).

%% @equiv gen_server:wait_response/2
-spec wait_response(RequestId :: request_id(), timeout()) ->
    {reply, Reply :: term()} | 'timeout' | {error, {Reason :: term(), server_ref()}}.
wait_response(RequestId, Timeout) ->
    gen:wait_response(RequestId, Timeout).

%% @equiv gen_server:check_response/2
-spec check_response(Msg :: term(), RequestId :: request_id()) ->
    {reply, Reply :: term()} | 'no_reply' | {error, {Reason :: term(), server_ref()}}.
check_response(Msg, RequestId) ->
    gen:check_response(Msg, RequestId).

%% -----------------------------------------------------------------
%% Make a cast to a generic server.
%% -----------------------------------------------------------------
%% @equiv gen_server:cast/2
cast(Dest, Request) ->
    do_send(Dest, cast_msg(Request)).

%% @doc 向默认接收地址发送一条cast消息, 名字和原有的重复了所以改成ecast, 进程内使用
-spec ecast(term()) -> ok.
ecast(Request) ->
    send(cast_msg(Request)).

%% @doc 向默认接收地址发送一条延时N毫秒的cast消息, 进程内使用
-spec cast_after(non_neg_integer(), term()) -> ok.
cast_after(After, Request) ->
    send_after(After, cast_msg(Request)).

%% @doc 发送一条延时N毫秒的cast消息, 进程内使用
-spec cast_after(non_neg_integer(), server_ref(), term()) -> ok.
cast_after(After, Dest, Request) ->
    send_after(After, Dest, cast_msg(Request)).

%% @doc 相当于在X毫秒时间戳(毫秒)向默认接收地址发送一条cast消息, 进程内使用
-spec cast_at(non_neg_integer(), term()) -> ok.
cast_at(ExceptTime, Request) ->
    send_at(ExceptTime, cast_msg(Request)).

%% @doc 相当于在X毫秒时间戳(毫秒)发送一条cast消息, 进程内使用
-spec cast_at(non_neg_integer(), server_ref(), term()) -> ok.
cast_at(ExceptTime, Dest, Request) ->
    send_at(ExceptTime, Dest, cast_msg(Request)).

%% @doc 发送一条cast消息同时指定期望时间, 进程内使用
-spec cast_after(non_neg_integer(), server_ref(), non_neg_integer(), term()) -> ok.
cast_after(After, Dest, ExceptTime, Request) ->
    send_after(After, Dest, ExceptTime, cast_msg(Request)).

%% @doc 马上向默认接收地址发送一条cast消息, 不可回滚, 进程内使用
-spec cast_immediately(term()) -> ok.
cast_immediately(Request) ->
    send_immediately(cast_msg(Request)).

%% @doc 马上发送一条cast消息, 不可回滚, 进程内使用
-spec cast_immediately(server_ref(), term()) -> ok.
cast_immediately(Dest, Request) ->
    send_immediately(Dest, cast_msg(Request)).

%% @doc 马上向默认接收地址发送一条延时N毫秒的cast消息, 不可回滚, 进程内使用
-spec cast_after_immediately(non_neg_integer(), term()) -> ok.
cast_after_immediately(After, Request) ->
    send_after_immediately(After, cast_msg(Request)).

%% @doc 马上发送一条延时N毫秒的cast消息, 不可回滚, 进程内使用
-spec cast_after_immediately(non_neg_integer(), server_ref(), term()) -> ok.
cast_after_immediately(After, Dest, Request) ->
    send_after_immediately(After, Dest, cast_msg(Request)).

%% @doc 相当于在X毫秒时间戳向默认接收地址发送一条cast消息, 不可回滚, 进程内使用
-spec cast_at_immediately(non_neg_integer(), term()) -> ok.
cast_at_immediately(ExceptTime, Request) ->
    send_at_immediately(ExceptTime, cast_msg(Request)).

%% @doc 相当于在X毫秒时间戳发送一条cast消息, 不可回滚, 进程内使用
-spec cast_at_immediately(non_neg_integer(), server_ref(), term()) -> ok.
cast_at_immediately(ExceptTime, Dest, Request) ->
    send_at_immediately(ExceptTime, Dest, cast_msg(Request)).

%% @doc 马上发送一条延时N毫秒的cast消息同时指定期望时间, 不可回滚, 进程内使用
-spec cast_after_immediately(non_neg_integer(), server_ref(), non_neg_integer(), term()) -> ok.
cast_after_immediately(After, Dest, ExceptTime, Request) ->
    send_after_immediately(After, Dest, ExceptTime, Request).

cast_msg(Request) -> {'$gen_cast', Request}.

%% -----------------------------------------------------------------
%% Send a msg.
%% -----------------------------------------------------------------
%% @doc 向默认接收地址发送一条消息, 进程内使用
-spec send(term()) -> ok.
send(Request) ->
    put(?PD_EXIA_SEND, [?EXIA_PREPARE_MSG(get(?PD_EXIA_DEST), undefined, (Request)) | get(?PD_EXIA_SEND)]),
    ok.

%% @doc 发送一条消息, 进程内使用
-spec send(non_neg_integer(), term()) -> ok.
send(Dest, Request) ->
    put(?PD_EXIA_SEND, [?EXIA_PREPARE_MSG(Dest, undefined, (Request)) | get(?PD_EXIA_SEND)]),
    ok.

%% @doc 向默认接收地址发送一条延时N毫秒的消息, 进程内使用
-spec send_after(non_neg_integer(), term()) -> ok.
send_after(After, Request) ->
    put(?PD_EXIA_SEND, [?EXIA_PREPARE_MSG(get(?PD_EXIA_DEST), get_expect_millisecond() + After, (Request)) | get(?PD_EXIA_SEND)]),
    ok.

%% @doc 发送一条延时N毫秒的消息, 进程内使用
-spec send_after(non_neg_integer(), server_ref(), term()) -> ok.
send_after(After, Dest, Request) ->
    put(?PD_EXIA_SEND, [?EXIA_PREPARE_MSG(Dest, get_expect_millisecond() + After, (Request)) | get(?PD_EXIA_SEND)]),
    ok.

%% @doc 相当于在X毫秒时间戳(毫秒)向默认接收地址发送一条消息, 进程内使用
-spec send_at(non_neg_integer(), term()) -> ok.
send_at(ExceptTime, Request) ->
    put(?PD_EXIA_SEND, [?EXIA_PREPARE_MSG(get(?PD_EXIA_DEST), ExceptTime, (Request)) | get(?PD_EXIA_SEND)]),
    ok.

%% @doc 相当于在X毫秒时间戳(毫秒)发送一条消息, 进程内使用
-spec send_at(non_neg_integer(), server_ref(), term()) -> ok.
send_at(ExceptTime, Dest, Request) ->
    put(?PD_EXIA_SEND, [?EXIA_PREPARE_MSG(Dest, ExceptTime, (Request)) | get(?PD_EXIA_SEND)]),
    ok.

%% @doc 发送一条消息同时指定期望时间, 进程内使用
-spec send_after(non_neg_integer(), server_ref(), non_neg_integer(), term()) -> ok.
send_after(After, Dest, ExceptTime, Request) ->
    put(?PD_EXIA_SEND, [?EXIA_PREPARE_MSG(After, Dest, ExceptTime, (Request)) | get(?PD_EXIA_SEND)]),
    ok.

%% @doc 马上向默认接收地址发送一条消息, 不可回滚, 进程内使用
-spec send_immediately(term()) -> ok.
send_immediately(Request) ->
    flush_msg([?EXIA_PREPARE_MSG(get(?PD_EXIA_DEST), undefined, (Request))], get_msg_millisecond()).

%% @doc 马上发送一条消息, 不可回滚, 进程内使用
-spec send_immediately(server_ref(), term()) -> ok.
send_immediately(Dest, Request) ->
    flush_msg([?EXIA_PREPARE_MSG(Dest, undefined, (Request))], get_msg_millisecond()).

%% @doc 马上向默认接收地址发送一条延时N毫秒的消息, 不可回滚, 进程内使用
-spec send_after_immediately(non_neg_integer(), term()) -> ok.
send_after_immediately(After, Request) ->
    flush_msg([?EXIA_PREPARE_MSG(get(?PD_EXIA_DEST), get_expect_millisecond() + After, (Request))], get_msg_millisecond()).

%% @doc 马上发送一条延时N毫秒的消息, 不可回滚, 进程内使用
-spec send_after_immediately(non_neg_integer(), server_ref(), term()) -> ok.
send_after_immediately(After, Dest, Request) ->
    flush_msg([?EXIA_PREPARE_MSG(Dest, get_expect_millisecond() + After, (Request))], get_msg_millisecond()).

%% @doc 相当于在X毫秒时间戳向默认接收地址发送一条消息, 不可回滚, 进程内使用
-spec send_at_immediately(non_neg_integer(), term()) -> ok.
send_at_immediately(ExceptTime, Request) ->
    flush_msg([?EXIA_PREPARE_MSG(get(?PD_EXIA_DEST), ExceptTime, (Request))], get_msg_millisecond()).

%% @doc 相当于在X毫秒时间戳发送一条消息, 不可回滚, 进程内使用
-spec send_at_immediately(non_neg_integer(), server_ref(), term()) -> ok.
send_at_immediately(ExceptTime, Dest, Request) ->
    flush_msg([?EXIA_PREPARE_MSG(Dest, ExceptTime, (Request))], get_msg_millisecond()).

%% @doc 马上发送一条延时N毫秒的消息同时指定期望时间, 不可回滚, 进程内使用
-spec send_after_immediately(non_neg_integer(), server_ref(), non_neg_integer(), term()) -> ok.
send_after_immediately(After, Dest, ExceptTime, Request) ->
    flush_msg([?EXIA_PREPARE_MSG(After, Dest, ExceptTime, (Request))], get_msg_millisecond()).

%% 刷新msg缓存
flush_msg() ->
    flush_msg(lists:reverse(get(?PD_EXIA_SEND)), get_msg_millisecond()),
    put(?PD_EXIA_SEND, []).

flush_msg([], _MsgMilliSecond) ->
    ok;
flush_msg([?EXIA_PREPARE_MSG(Dest, undefined, Request) | T], MsgMilliSecond) ->
    do_send(Dest, #exia_msg{expect_time = get_expect_millisecond(), msg = Request}),
    flush_msg(T, MsgMilliSecond);
flush_msg([?EXIA_PREPARE_MSG(Dest, ExceptTime, Request) | T], MsgMilliSecond) ->
    case MsgMilliSecond >= ExceptTime of
        true ->
            %% 期望时间比真实时间还要早, 马上发
            do_send(Dest, #exia_msg{expect_time = ExceptTime, msg = Request});
        _ ->
            After = ExceptTime - MsgMilliSecond,
            erlang:send_after(After, Dest, #exia_msg{expect_time = ExceptTime, msg = Request})
    end,
    flush_msg(T, MsgMilliSecond);
flush_msg([?EXIA_PREPARE_MSG(After, Dest, ExceptTime, Request) | T], MsgMilliSecond) ->
    erlang:send_after(After, Dest, #exia_msg{expect_time = ExceptTime, msg = Request}),
    flush_msg(T, MsgMilliSecond).

do_send({global, Name}, Request) ->
    catch global:send(Name, Request),
    ok;
do_send({via, Mod, Name}, Request) ->
    catch Mod:send(Name, Request),
    ok;
do_send({Name, Node} = Dest, Request) when is_atom(Name), is_atom(Node) ->
    do_normal_send(Dest, Request);
do_send(Dest, Request) when is_atom(Dest) ->
    do_normal_send(Dest, Request);
do_send(Dest, Request) when is_pid(Dest) ->
    do_normal_send(Dest, Request).

%% -----------------------------------------------------------------
%% Send a reply to the client.
%% -----------------------------------------------------------------
%% @equiv gen_server:rely/2
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply},
    ok.

%% -----------------------------------------------------------------
%% Asynchronous broadcast, returns nothing, it's just send 'n' pray
%%-----------------------------------------------------------------
%% @equiv gen_server:abcast/2
abcast(Name, Request) when is_atom(Name) ->
    do_abcast([node() | nodes()], Name, cast_msg(Request)).

%% @equiv gen_server:abcast/3
abcast(Nodes, Name, Request) when is_list(Nodes), is_atom(Name) ->
    do_abcast(Nodes, Name, cast_msg(Request)).

do_abcast([Node | Nodes], Name, Msg) when is_atom(Node) ->
    do_normal_send({Name, Node}, Msg),
    do_abcast(Nodes, Name, Msg);
do_abcast([], _, _) -> abcast.

%%% -----------------------------------------------------------------
%%% Make a call to servers at several nodes.
%%% Returns: {[Replies],[BadNodes]}
%%% A Timeout can be given
%%%
%%% A middleman process is used in case late answers arrives after
%%% the timeout. If they would be allowed to glog the callers message
%%% queue, it would probably become confused. Late answers will
%%% now arrive to the terminated middleman and so be discarded.
%%% -----------------------------------------------------------------
%% @equiv gen_server:multi_call/2
multi_call(Name, Req)
    when is_atom(Name) ->
    do_multi_call([node() | nodes()], Name, Req, infinity).

%% @equiv gen_server:multi_call/3
multi_call(Nodes, Name, Req)
    when is_list(Nodes), is_atom(Name) ->
    do_multi_call(Nodes, Name, Req, infinity).

%% @equiv gen_server:multi_call/4
multi_call(Nodes, Name, Req, infinity) ->
    do_multi_call(Nodes, Name, Req, infinity);
multi_call(Nodes, Name, Req, Timeout)
    when is_list(Nodes), is_atom(Name), is_integer(Timeout), Timeout >= 0 ->
    do_multi_call(Nodes, Name, Req, Timeout).


%%% -----------------------------------------------------------------
%% 时间相关
%%% -----------------------------------------------------------------
%% @doc 获取期望时间, 毫秒, 进程内使用
-spec get_expect_millisecond() -> non_neg_integer().
get_expect_millisecond() ->
    get(?PD_EXIA_EXPECT_TIME).

%% @doc 获取期望时间, 秒, 进程内使用
-spec get_expect_second() -> non_neg_integer().
get_expect_second() ->
    get(?PD_EXIA_EXPECT_TIME) div 1000.

%% @doc 获取消息时间, 毫秒, 进程内使用
-spec get_msg_millisecond() -> non_neg_integer().
get_msg_millisecond() ->
    get(?PD_EXIA_MSG_TIME).

%% @doc 获取消息时间, 秒, 进程内使用
-spec get_msg_second() -> non_neg_integer().
get_msg_second() ->
    get(?PD_EXIA_MSG_TIME) div 1000.

%%% -----------------------------------------------------------------
%% 模拟进程字典
%%% -----------------------------------------------------------------
%% @doc 模拟进程字典获取value, 进程内使用
-spec eget(term(), term()) -> ok.
eget(Key, Default) ->
    maps:get(Key, get(?PD_EXIA_PD), Default),
    ok.

%% @doc 模拟进程字典设置value, 但不会返回以前的值, 进程内使用
-spec eset(term(), term()) -> ok.
eset(Key, Value) ->
    put(?PD_EXIA_PD, maps:put(Key, Value, get(?PD_EXIA_PD))),
    ok.

%%-----------------------------------------------------------------
%% enter_loop(Mod, Options, State, <ServerName>, <TimeOut>) ->_
%%
%% Description: Makes an existing process into a gen_server.
%%              The calling process will enter the gen_server receive
%%              loop and become a gen_server process.
%%              The process *must* have been started using one of the
%%              start functions in proc_lib, see proc_lib(3).
%%              The user is responsible for any initialization of the
%%              process, including registering a name for it.
%%-----------------------------------------------------------------
%% @equiv gen_server:enter_loop/3
enter_loop(Mod, Options, State) ->
    enter_loop(Mod, Options, State, self(), infinity).

%% @equiv gen_server:enter_loop/4
enter_loop(Mod, Options, State, ServerName = {Scope, _})
    when Scope == local; Scope == global ->
    enter_loop(Mod, Options, State, ServerName, infinity);

enter_loop(Mod, Options, State, ServerName = {via, _, _}) ->
    enter_loop(Mod, Options, State, ServerName, infinity);

enter_loop(Mod, Options, State, Timeout) ->
    enter_loop(Mod, Options, State, self(), Timeout).

%% @equiv gen_server:enter_loop/5
enter_loop(Mod, Options, State, ServerName, Timeout) ->
    Name = gen:get_proc_name(ServerName),
    Parent = gen:get_parent(),
    Debug = gen:debug_options(Name, Options),
    HibernateAfterTimeout = gen:hibernate_after(Options),
    init_pd(),
    loop(Parent, Name, State, Mod, Timeout, HibernateAfterTimeout, Debug).

%%%========================================================================
%%% Gen-callback functions
%%%========================================================================

%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
%% @private
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = gen:name(Name0),
    Debug = gen:debug_options(Name, Options),
    HibernateAfterTimeout = gen:hibernate_after(Options),
    init_pd(),

    case init_it(Mod, Args) of
        {ok, {ok, State}} ->
            proc_lib:init_ack(Starter, {ok, self()}),
            after_msg(),
            loop(Parent, Name, State, Mod, infinity, HibernateAfterTimeout, Debug);
        {ok, {ok, State, Timeout}} ->
            proc_lib:init_ack(Starter, {ok, self()}),
            after_msg(),
            loop(Parent, Name, State, Mod, Timeout, HibernateAfterTimeout, Debug);
        {ok, {stop, Reason}} ->
            %% For consistency, we must make sure that the
            %% registered name (if any) is unregistered before
            %% the parent process is notified about the failure.
            %% (Otherwise, the parent process could get
            %% an 'already_started' error if it immediately
            %% tried starting the process again.)
            gen:unregister_name(Name0),
            proc_lib:init_ack(Starter, {error, Reason}),
            after_msg(),
            exit(Reason);
        {ok, ignore} ->
            gen:unregister_name(Name0),
            proc_lib:init_ack(Starter, ignore),
            after_msg(),
            exit(normal);
        {ok, Else} ->
            Error = {bad_return_value, Else},
            proc_lib:init_ack(Starter, {error, Error}),
            after_msg(),
            exit(Error);
        {'EXIT', Class, Reason, Stacktrace} ->
            gen:unregister_name(Name0),
            proc_lib:init_ack(Starter, {error, terminate_reason(Class, Reason, Stacktrace)}),
            %% 报错的时候不发消息
            erlang:raise(Class, Reason, Stacktrace)
    end.
init_it(Mod, Args) ->
    try
        {ok, Mod:init(Args)}
    catch
        throw:{exia_private, return, R} -> {ok, R};
        Class:R:S -> {'EXIT', Class, R, S}
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% ---------------------------------------------------
%%% The MAIN loop.
%%% ---------------------------------------------------

loop(Parent, Name, State, Mod, {continue, Continue} = Msg, HibernateAfterTimeout, Debug) ->
    {Msg1, Rollback} = before_msg(Continue, State),
    Reply = try_dispatch(Mod, handle_continue, Msg1, State),
    case Debug of
        [] ->
            handle_common_reply(Reply, Parent, Name, undefined, Msg, Mod,
                HibernateAfterTimeout, State, Rollback);
        _ ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name, Msg),
            handle_common_reply(Reply, Parent, Name, undefined, Msg, Mod,
                HibernateAfterTimeout, State, Debug1, Rollback)
    end;

loop(Parent, Name, State, Mod, hibernate, HibernateAfterTimeout, Debug) ->
    proc_lib:hibernate(?MODULE, wake_hib, [Parent, Name, State, Mod, HibernateAfterTimeout, Debug]);

loop(Parent, Name, State, Mod, infinity, HibernateAfterTimeout, Debug) ->
    receive
        Msg ->
            decode_msg(Msg, Parent, Name, State, Mod, infinity, HibernateAfterTimeout, Debug, false)
    after HibernateAfterTimeout ->
        loop(Parent, Name, State, Mod, hibernate, HibernateAfterTimeout, Debug)
    end;

loop(Parent, Name, State, Mod, Time, HibernateAfterTimeout, Debug) ->
    Msg = receive
              Input ->
                  Input
          after Time ->
            timeout
          end,
    decode_msg(Msg, Parent, Name, State, Mod, Time, HibernateAfterTimeout, Debug, false).

%% @equiv gen_server:wake_hib/6
wake_hib(Parent, Name, State, Mod, HibernateAfterTimeout, Debug) ->
    Msg = receive
              Input ->
                  Input
          end,
    decode_msg(Msg, Parent, Name, State, Mod, hibernate, HibernateAfterTimeout, Debug, true).

decode_msg(Msg, Parent, Name, State, Mod, Time, HibernateAfterTimeout, Debug, Hib) ->
    case Msg of
        {system, From, Req} ->
            sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
                [Name, State, Mod, Time, HibernateAfterTimeout], Hib);
        {'EXIT', Parent, Reason} ->
            Rollback = before_msg(State),
            terminate(Reason, ?STACKTRACE(), Name, undefined, Msg, Mod, State, Rollback, Debug);
        _Msg when Debug =:= [] ->
            handle_msg(Msg, Parent, Name, State, Mod, HibernateAfterTimeout);
        _Msg ->
            Debug1 = sys:handle_debug(Debug, fun print_event/3,
                Name, {in, Msg}),
            handle_msg(Msg, Parent, Name, State, Mod, HibernateAfterTimeout, Debug1)
    end.

%%% ---------------------------------------------------
%%% Send/receive functions
%%% ---------------------------------------------------
do_normal_send(Dest, Msg) ->
    try erlang:send(Dest, Msg)
    catch
        error:_ -> ok
    end,
    ok.

do_multi_call(Nodes, Name, Req, infinity) ->
    Tag = make_ref(),
    Monitors = send_nodes(Nodes, Name, Tag, Req),
    rec_nodes(Tag, Monitors, Name, undefined);
do_multi_call(Nodes, Name, Req, Timeout) ->
    Tag = make_ref(),
    Caller = self(),
    Receiver =
        spawn(
            fun() ->
                %% Middleman process. Should be unsensitive to regular
                %% exit signals. The sychronization is needed in case
                %% the receiver would exit before the caller started
                %% the monitor.
                process_flag(trap_exit, true),
                Mref = erlang:monitor(process, Caller),
                receive
                    {Caller, Tag} ->
                        Monitors = send_nodes(Nodes, Name, Tag, Req),
                        TimerId = erlang:start_timer(Timeout, self(), ok),
                        Result = rec_nodes(Tag, Monitors, Name, TimerId),
                        exit({self(), Tag, Result});
                    {'DOWN', Mref, _, _, _} ->
                        %% Caller died before sending us the go-ahead.
                        %% Give up silently.
                        exit(normal)
                end
            end),
    Mref = erlang:monitor(process, Receiver),
    Receiver ! {self(), Tag},
    receive
        {'DOWN', Mref, _, _, {Receiver, Tag, Result}} ->
            Result;
        {'DOWN', Mref, _, _, Reason} ->
            %% The middleman code failed. Or someone did
            %% exit(_, kill) on the middleman process => Reason==killed
            exit(Reason)
    end.

send_nodes(Nodes, Name, Tag, Req) ->
    send_nodes(Nodes, Name, Tag, Req, []).

send_nodes([Node | Tail], Name, Tag, Req, Monitors)
    when is_atom(Node) ->
    Monitor = start_monitor(Node, Name),
    %% Handle non-existing names in rec_nodes.
    catch {Name, Node} ! {'$gen_call', {self(), {Tag, Node}}, Req},
    send_nodes(Tail, Name, Tag, Req, [Monitor | Monitors]);
send_nodes([_Node | Tail], Name, Tag, Req, Monitors) ->
    %% Skip non-atom Node
    send_nodes(Tail, Name, Tag, Req, Monitors);
send_nodes([], _Name, _Tag, _Req, Monitors) ->
    Monitors.

%% Against old nodes:
%% If no reply has been delivered within 2 secs. (per node) check that
%% the server really exists and wait for ever for the answer.
%%
%% Against contemporary nodes:
%% Wait for reply, server 'DOWN', or timeout from TimerId.

rec_nodes(Tag, Nodes, Name, TimerId) ->
    rec_nodes(Tag, Nodes, Name, [], [], 2000, TimerId).

rec_nodes(Tag, [{N, R} | Tail], Name, Badnodes, Replies, Time, TimerId) ->
    receive
        {'DOWN', R, _, _, _} ->
            rec_nodes(Tag, Tail, Name, [N | Badnodes], Replies, Time, TimerId);
        {{Tag, N}, Reply} ->  %% Tag is bound !!!
            erlang:demonitor(R, [flush]),
            rec_nodes(Tag, Tail, Name, Badnodes,
                [{N, Reply} | Replies], Time, TimerId);
        {timeout, TimerId, _} ->
            erlang:demonitor(R, [flush]),
            %% Collect all replies that already have arrived
            rec_nodes_rest(Tag, Tail, Name, [N | Badnodes], Replies)
    end;
rec_nodes(Tag, [N | Tail], Name, Badnodes, Replies, Time, TimerId) ->
    %% R6 node
    receive
        {nodedown, N} ->
            monitor_node(N, false),
            rec_nodes(Tag, Tail, Name, [N | Badnodes], Replies, 2000, TimerId);
        {{Tag, N}, Reply} ->  %% Tag is bound !!!
            receive {nodedown, N} -> ok after 0 -> ok end,
            monitor_node(N, false),
            rec_nodes(Tag, Tail, Name, Badnodes,
                [{N, Reply} | Replies], 2000, TimerId);
        {timeout, TimerId, _} ->
            receive {nodedown, N} -> ok after 0 -> ok end,
            monitor_node(N, false),
            %% Collect all replies that already have arrived
            rec_nodes_rest(Tag, Tail, Name, [N | Badnodes], Replies)
    after Time ->
        case rpc:call(N, erlang, whereis, [Name]) of
            Pid when is_pid(Pid) -> % It exists try again.
                rec_nodes(Tag, [N | Tail], Name, Badnodes,
                    Replies, infinity, TimerId);
            _ -> % badnode
                receive {nodedown, N} -> ok after 0 -> ok end,
                monitor_node(N, false),
                rec_nodes(Tag, Tail, Name, [N | Badnodes],
                    Replies, 2000, TimerId)
        end
    end;
rec_nodes(_, [], _, Badnodes, Replies, _, TimerId) ->
    case catch erlang:cancel_timer(TimerId) of
        false ->  % It has already sent it's message
            receive
                {timeout, TimerId, _} -> ok
            after 0 ->
                ok
            end;
        _ -> % Timer was cancelled, or TimerId was 'undefined'
            ok
    end,
    {Replies, Badnodes}.

%% Collect all replies that already have arrived
rec_nodes_rest(Tag, [{N, R} | Tail], Name, Badnodes, Replies) ->
    receive
        {'DOWN', R, _, _, _} ->
            rec_nodes_rest(Tag, Tail, Name, [N | Badnodes], Replies);
        {{Tag, N}, Reply} -> %% Tag is bound !!!
            erlang:demonitor(R, [flush]),
            rec_nodes_rest(Tag, Tail, Name, Badnodes, [{N, Reply} | Replies])
    after 0 ->
        erlang:demonitor(R, [flush]),
        rec_nodes_rest(Tag, Tail, Name, [N | Badnodes], Replies)
    end;
rec_nodes_rest(Tag, [N | Tail], Name, Badnodes, Replies) ->
    %% R6 node
    receive
        {nodedown, N} ->
            monitor_node(N, false),
            rec_nodes_rest(Tag, Tail, Name, [N | Badnodes], Replies);
        {{Tag, N}, Reply} ->  %% Tag is bound !!!
            receive {nodedown, N} -> ok after 0 -> ok end,
            monitor_node(N, false),
            rec_nodes_rest(Tag, Tail, Name, Badnodes, [{N, Reply} | Replies])
    after 0 ->
        receive {nodedown, N} -> ok after 0 -> ok end,
        monitor_node(N, false),
        rec_nodes_rest(Tag, Tail, Name, [N | Badnodes], Replies)
    end;
rec_nodes_rest(_Tag, [], _Name, Badnodes, Replies) ->
    {Replies, Badnodes}.


%%% ---------------------------------------------------
%%% Monitor functions
%%% ---------------------------------------------------

start_monitor(Node, Name) when is_atom(Node), is_atom(Name) ->
    if node() =:= nonode@nohost, Node =/= nonode@nohost ->
        Ref = make_ref(),
        self() ! {'DOWN', Ref, process, {Name, Node}, noconnection},
        {Node, Ref};
        true ->
            case catch erlang:monitor(process, {Name, Node}) of
                {'EXIT', _} ->
                    %% Remote node is R6
                    monitor_node(Node, true),
                    Node;
                Ref when is_reference(Ref) ->
                    {Node, Ref}
            end
    end.

%% ---------------------------------------------------
%% Helper functions for try-catch of callbacks.
%% Returns the return value of the callback, or
%% {'EXIT', Class, Reason, Stack} (if an exception occurs)
%%
%% The Class, Reason and Stack are given to erlang:raise/3
%% to make sure proc_lib receives the proper reasons and
%% stacktraces.
%% ---------------------------------------------------

try_dispatch({'$gen_cast', {exia_private, execute, Execute}}, _Mod, State) ->
    try
        {ok, prim_execute(Execute, State)}
    catch
        throw:{exia_private, return, R} -> {ok, R};
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end;
try_dispatch({'$gen_cast', Msg}, Mod, State) ->
    try_dispatch(Mod, handle_cast, Msg, State);
try_dispatch(Info, Mod, State) ->
    try_dispatch(Mod, handle_info, Info, State).

try_dispatch(Mod, Func, Msg, State) ->
    try
        {ok, Mod:Func(Msg, State)}
    catch
        throw:{exia_private, return, R} -> {ok, R};
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end.

try_handle_call(_Mod, {exia_private, get_dest}, _From, State) ->
    {ok, {reply, get(?PD_EXIA_DEST), State}};
try_handle_call(_Mod, {exia_private, set_dest, Dest}, _From, State) ->
    {ok, {reply, put(?PD_EXIA_DEST, Dest), State}};
try_handle_call(_Mod, {exia_private, execute, Execute}, _From, State) ->
    try
        {ok, prim_execute(Execute, State)}
    catch
        throw:{exia_private, return, R} -> {ok, R};
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end;
try_handle_call(Mod, #exia_msg{msg = Msg}, From, State) ->
    try
        {ok, Mod:handle_call(Msg, From, State)}
    catch
        throw:{exia_private, return, R} -> {ok, R};
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end;
try_handle_call(Mod, Msg, From, State) ->
    try
        {ok, Mod:handle_call(Msg, From, State)}
    catch
        throw:{exia_private, return, R} -> {ok, R};
        Class:R:Stacktrace ->
            {'EXIT', Class, R, Stacktrace}
    end.

prim_execute(Execute, State) ->
    case Execute of
        {M, F, A} when is_atom(M), is_atom(F), is_list(A) ->
            apply(M, F, A ++ [State]);
        {M, F} when is_atom(M), is_atom(F) ->
            apply(M, F, [State]);
        {F, A} when is_function(F), is_list(A) ->
            apply(F, A ++ [State]);
        F when is_function(F) ->
            apply(F, [State])
    end.

try_terminate(Mod, Reason, State) ->
    case erlang:function_exported(Mod, terminate, 2) of
        true ->
            try
                {ok, Mod:terminate(Reason, State)}
            catch
                throw:{exia_private, return, R} -> {ok, R};
                Class:R:Stacktrace ->
                    {'EXIT', Class, R, Stacktrace}
            end;
        false ->
            {ok, ok}
    end.


%%% ---------------------------------------------------
%%% Message handling functions
%%% ---------------------------------------------------

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, HibernateAfterTimeout) ->
    {Msg1, Rollback} = before_msg(Msg, State),
    Result = try_handle_call(Mod, Msg1, From, State),
    case Result of
        {ok, {reply, Reply, NState}} ->
            after_msg(),
            reply(From, Reply),
            loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, []);
        {ok, {reply, Reply, NState, Time1}} ->
            after_msg(),
            reply(From, Reply),
            loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, []);
        {ok, {noreply, NState}} ->
            after_msg(),
            loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, []);
        {ok, {noreply, NState, Time1}} ->
            after_msg(),
            loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, []);
        {ok, {stop, Reason, Reply, NState}} ->
            flush(NState),
            try
                terminate(Reason, ?STACKTRACE(), Name, From, Msg1, Mod, NState, Rollback, [])
            after
                reply(From, Reply)
            end;
        Other -> handle_common_reply(Other, Parent, Name, From, Msg1, Mod, HibernateAfterTimeout, State, Rollback)
    end;
handle_msg(Msg, Parent, Name, State, Mod, HibernateAfterTimeout) ->
    {Msg1, Rollback} = before_msg(Msg, State),
    Reply = try_dispatch(Msg1, Mod, State),
    handle_common_reply(Reply, Parent, Name, undefined, Msg1, Mod, HibernateAfterTimeout, State, Rollback).

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, HibernateAfterTimeout, Debug) ->
    {Msg1, Rollback} = before_msg(Msg, State),
    Result = try_handle_call(Mod, Msg1, From, State),
    case Result of
        {ok, {reply, Reply, NState}} ->
            after_msg(),
            Debug1 = reply(Name, From, Reply, NState, Debug),
            loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, Debug1);
        {ok, {reply, Reply, NState, Time1}} ->
            after_msg(),
            Debug1 = reply(Name, From, Reply, NState, Debug),
            loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, Debug1);
        {ok, {noreply, NState}} ->
            after_msg(),
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                {noreply, NState}),
            loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, Debug1);
        {ok, {noreply, NState, Time1}} ->
            after_msg(),
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                {noreply, NState}),
            loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, Debug1);
        {ok, {stop, Reason, Reply, NState}} ->
            flush(NState),
            try
                terminate(Reason, ?STACKTRACE(), Name, From, Msg1, Mod, NState, Rollback, Debug)
            after
                _ = reply(Name, From, Reply, NState, Debug)
            end;
        Other ->
            handle_common_reply(Other, Parent, Name, From, Msg1, Mod, HibernateAfterTimeout, State, Debug, Rollback)
    end;
handle_msg(Msg, Parent, Name, State, Mod, HibernateAfterTimeout, Debug) ->
    {Msg1, Rollback} = before_msg(Msg, State),
    Reply = try_dispatch(Msg1, Mod, State),
    handle_common_reply(Reply, Parent, Name, undefined, Msg1, Mod, HibernateAfterTimeout, State, Debug, Rollback).

handle_common_reply(Reply, Parent, Name, From, Msg, Mod, HibernateAfterTimeout, _State, Rollback) ->
    case Reply of
        {ok, {noreply, NState}} ->
            after_msg(),
            loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, []);
        {ok, {noreply, NState, Time1}} ->
            after_msg(),
            loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, []);
        {ok, {stop, Reason, NState}} ->
            flush(NState),
            terminate(Reason, ?STACKTRACE(), Name, From, Msg, Mod, NState, Rollback, []);
        %% stop是唯一关闭进程的方法
        {'EXIT', Class, Reason, Stacktrace} ->
            ?LOG_ERROR("~p, ~p~n~p", [Class, Reason, Stacktrace]),
            RollbackState = rollback(Rollback),
            loop(Parent, Name, RollbackState, Mod, infinity, HibernateAfterTimeout, []);
        {ok, BadReply} ->
            ?LOG_ERROR("~p~n~p", [bad_return_value, BadReply]),
            RollbackState = rollback(Rollback),
            loop(Parent, Name, RollbackState, Mod, infinity, HibernateAfterTimeout, [])
    end.

handle_common_reply(Reply, Parent, Name, From, Msg, Mod, HibernateAfterTimeout, State, Debug, Rollback) ->
    case Reply of
        {ok, {noreply, NState}} ->
            after_msg(),
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                {noreply, NState}),
            loop(Parent, Name, NState, Mod, infinity, HibernateAfterTimeout, Debug1);
        {ok, {noreply, NState, Time1}} ->
            after_msg(),
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                {noreply, NState}),
            loop(Parent, Name, NState, Mod, Time1, HibernateAfterTimeout, Debug1);
        {ok, {stop, Reason, NState}} ->
            flush(NState),
            terminate(Reason, ?STACKTRACE(), Name, From, Msg, Mod, NState, Rollback, Debug);
        %% stop是唯一关闭进程的方法
        {'EXIT', Class, Reason, Stacktrace} ->
            ?LOG_ERROR("~p, ~p~n~p", [Class, Reason, Stacktrace]),
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                {noreply, State}),
            RollbackState = rollback(Rollback),
            loop(Parent, Name, RollbackState, Mod, infinity, HibernateAfterTimeout, Debug1);
        {ok, BadReply} ->
            ?LOG_ERROR("~p~n~p", [bad_return_value, BadReply]),
            Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
                {noreply, State}),
            RollbackState = rollback(Rollback),
            loop(Parent, Name, RollbackState, Mod, infinity, HibernateAfterTimeout, Debug1)
    end.

reply(Name, From, Reply, State, Debug) ->
    reply(From, Reply),
    sys:handle_debug(Debug, fun print_event/3, Name,
        {out, Reply, From, State}).

%% 初始化进程字典
init_pd() ->
    put(?PD_EXIA_SEND, []),
    put(?PD_EXIA_PD, #{}),
    Now = erlang:system_time(millisecond),
    put(?PD_EXIA_MSG_TIME, Now),
    put(?PD_EXIA_EXPECT_TIME, Now),
    vmysql:process_init().

%% 处理消息前后
before_msg(State) ->
    Now = erlang:system_time(millisecond),
    put(?PD_EXIA_MSG_TIME, Now),
    put(?PD_EXIA_EXPECT_TIME, Now),
    hold(State).

before_msg(#exia_msg{msg = Msg, expect_time = ExpectTime}, State) ->
    Now = erlang:system_time(millisecond),
    put(?PD_EXIA_MSG_TIME, Now),
    put(?PD_EXIA_EXPECT_TIME, ExpectTime),
    {Msg, hold(State)};
before_msg(Msg, State) ->
    Now = erlang:system_time(millisecond),
    put(?PD_EXIA_MSG_TIME, Now),
    put(?PD_EXIA_EXPECT_TIME, Now),
    {Msg, hold(State)}.

after_msg() ->
    flush_msg(),
    vmysql:sync_to_ets(),
    put(?PD_EXIA_ROLLBACK, undefined).

%% @doc 刷新缓存, 保存回滚点, 进程内使用
-spec flush(term()) -> ok.
flush(State) ->
    flush_msg(),
    vmysql:sync_to_ets(),
    put(?PD_EXIA_ROLLBACK, hold(State)),
    ok.

%% @doc 通过throw返回, 注意不要被catch, 进程内使用
-spec return(term()) -> term().
return(Return) ->
    throw({exia_private, return, Return}).

%% @doc 下层return若被上层catch到, 可以用该函数获取return内容, 进程内使用
-spec get_return(term()) -> error| {ok, Return :: term()}.
get_return({exia_private, return, Return}) -> {ok, Return};
get_return(_) -> error.

%%-----------------------------------------------------------------
%% 回滚相关
%%-----------------------------------------------------------------
hold(State) ->
    #exia_rollback{
        state = State,
        dest = get(?PD_EXIA_DEST),
        send = get(?PD_EXIA_SEND),
        virture = vmysql:hold(),
        pd = get(?PD_EXIA_PD)
    }.

rollback(Rollback) ->
    case get(?PD_EXIA_ROLLBACK) of
        undefined ->
            #exia_rollback{state = State, dest = Dest, send = Send, virture = Virture, pd = PD} = Rollback;
        #exia_rollback{state = State, dest = Dest, send = Send, virture = Virture, pd = PD} ->
            put(?PD_EXIA_ROLLBACK, undefined)
    end,
    put(?PD_EXIA_DEST, Dest),
    put(?PD_EXIA_SEND, Send),
    vmysql:rollback(Virture),
    put(?PD_EXIA_PD, PD),
    State.

after_terminate() ->
    flush_msg(),
    vmysql:sync_to_db().

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
%% @private
system_continue(Parent, Debug, [Name, State, Mod, Time, HibernateAfterTimeout]) ->
    loop(Parent, Name, State, Mod, Time, HibernateAfterTimeout, Debug).

%% @private
-spec system_terminate(_, _, _, [_]) -> no_return().
system_terminate(Reason, _Parent, Debug, [Name, State, Mod, _Time, _HibernateAfterTimeout]) ->
    Rollback = before_msg(State),
    terminate(Reason, ?STACKTRACE(), Name, undefined, [], Mod, State, Rollback, Debug).

%% @private
system_code_change([Name, State, Mod, Time, HibernateAfterTimeout], _Module, OldVsn, Extra) ->
    Rollback = before_msg(State),
    case catch Mod:code_change(OldVsn, State, Extra) of
        {ok, NewState} ->
            after_msg(),
            {ok, [Name, NewState, Mod, Time, HibernateAfterTimeout]};
        {exia_private, return, NewState} ->
            after_msg(),
            {ok, [Name, NewState, Mod, Time, HibernateAfterTimeout]};
        Else ->
            %% 进程字典回滚
            rollback(Rollback),
            Else
    end.

%% @private
system_get_state([_Name, State, _Mod, _Time, _HibernateAfterTimeout]) ->
    {ok, State}.

%% @private
system_replace_state(StateFun, [Name, State, Mod, Time, HibernateAfterTimeout]) ->
    Rollback = before_msg(State),
    try
        NewState = StateFun(State),
        after_msg(),
        {ok, NewState, [Name, NewState, Mod, Time, HibernateAfterTimeout]}
    catch
        throw:{exia_private, return, NState} ->
            after_msg(),
            {ok, NState, [Name, NState, Mod, Time, HibernateAfterTimeout]};
        Class:Reason:Stacktrace ->
            rollback(Rollback),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
        {'$gen_call', {From, _Tag}, Call} ->
            io:format(Dev, "*DBG* ~tp got call ~tp from ~tw~n",
                [Name, Call, From]);
        {'$gen_cast', Cast} ->
            io:format(Dev, "*DBG* ~tp got cast ~tp~n",
                [Name, Cast]);
        _ ->
            io:format(Dev, "*DBG* ~tp got ~tp~n", [Name, Msg])
    end;
print_event(Dev, {out, Msg, {To, _Tag}, State}, Name) ->
    io:format(Dev, "*DBG* ~tp sent ~tp to ~tw, new state ~tp~n",
        [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
    io:format(Dev, "*DBG* ~tp new state ~tp~n", [Name, State]);
print_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~tp dbg  ~tp~n", [Name, Event]).


%%% ---------------------------------------------------
%%% Terminate the server.
%%%
%%% terminate/8 is triggered by {stop, Reason} or bad
%%% return values. The stacktrace is generated via the
%%% ?STACKTRACE() macro and the ReportReason must not
%%% be wrapped in tuples.
%%%
%%% terminate/9 is triggered in case of error/exit in
%%% the user callback. In this case the report reason
%%% always includes the user stacktrace.
%%%
%%% The reason received in the terminate/2 callbacks
%%% always includes the stacktrace for errors and never
%%% for exits.
%%% ---------------------------------------------------

-spec terminate(_, _, _, _, _, _, _, _, _) -> no_return().
terminate(Reason, Stacktrace, Name, From, Msg, Mod, State, Rollback, Debug) ->
    terminate(exit, Reason, Stacktrace, Reason, Name, From, Msg, Mod, State, Rollback, Debug).

-spec terminate(_, _, _, _, _, _, _, _, _, _, _) -> no_return().
terminate(Class, Reason, Stacktrace, ReportReason, Name, From, Msg, Mod, State, Rollback, Debug) ->
    Reply = try_terminate(Mod, terminate_reason(Class, Reason, Stacktrace), State),
    case Reply of
        {'EXIT', C, R, S} ->
            RollbackState = rollback(Rollback),
            error_info({R, S}, Name, From, Msg, Mod, RollbackState, Debug),
            erlang:raise(C, R, S);
        _ ->
            case {Class, Reason} of
                {exit, normal} -> ok;
                {exit, shutdown} -> ok;
                {exit, {shutdown, _}} -> ok;
                _ ->
                    RollbackState = rollback(Rollback),
                    error_info(ReportReason, Name, From, Msg, Mod, RollbackState, Debug)
            end
    end,
    after_terminate(),
    case Stacktrace of
        [] ->
            erlang:Class(Reason);
        _ ->
            erlang:raise(Class, Reason, Stacktrace)
    end.

terminate_reason(throw, Reason, Stacktrace) -> {Reason, Stacktrace};
terminate_reason(error, Reason, Stacktrace) -> {Reason, Stacktrace};
terminate_reason(exit, Reason, _Stacktrace) -> Reason.

error_info(_Reason, application_controller, _From, _Msg, _Mod, _State, _Debug) ->
    %% OTP-5811 Don't send an error report if it's the system process
    %% application_controller which is terminating - let init take care
    %% of it instead
    ok;
error_info(Reason, Name, From, Msg, Mod, State, Debug) ->
    Log = sys:get_log(Debug),
    ?LOG_ERROR("~p", #{label => {gen_server, terminate},
        name => Name,
        last_message => Msg,
        state => format_status(terminate, Mod, get(), State),
        log => format_log_state(Mod, Log),
        reason => Reason,
        client_info => client_stacktrace(From)}),
    ok.

client_stacktrace(undefined) ->
    undefined;
client_stacktrace({From, _Tag}) ->
    client_stacktrace(From);
client_stacktrace(From) when is_pid(From), node(From) =:= node() ->
    case process_info(From, [current_stacktrace, registered_name]) of
        undefined ->
            {From, dead};
        [{current_stacktrace, Stacktrace}, {registered_name, []}] ->
            {From, {From, Stacktrace}};
        [{current_stacktrace, Stacktrace}, {registered_name, Name}] ->
            {From, {Name, Stacktrace}}
    end;
client_stacktrace(From) when is_pid(From) ->
    {From, remote}.

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
%% @private
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [Name, State, Mod, _Time, _HibernateAfterTimeout]] = StatusData,
    Header = gen:format_status_header("Status for generic server", Name),
    Log = sys:get_log(Debug),
    Specific = case format_status(Opt, Mod, PDict, State) of
                   S when is_list(S) -> S;
                   S -> [S]
               end,
    [{header, Header},
        {data, [{"Status", SysState},
            {"Parent", Parent},
            {"Logged events", format_log_state(Mod, Log)}]} |
        Specific].

format_log_state(Mod, Log) ->
    [case Event of
         {out, Msg, From, State} ->
             {out, Msg, From, format_status(terminate, Mod, get(), State)};
         {noreply, State} ->
             {noreply, format_status(terminate, Mod, get(), State)};
         _ -> Event
     end || Event <- Log].

format_status(Opt, Mod, PDict, State) ->
    DefStatus = case Opt of
                    terminate -> State;
                    _ -> [{data, [{"State", State}]}]
                end,
    case erlang:function_exported(Mod, format_status, 2) of
        true ->
            case catch Mod:format_status(Opt, [PDict, State]) of
                {'EXIT', _} -> DefStatus;
                Else -> Else
            end;
        _ ->
            DefStatus
    end.
