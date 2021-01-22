%% @private auto create
-module(gateway_1_handle).

-include("ptolemaios_lib.hrl").
-include("ec.hrl").
-include("gateway_1_pb.hrl").
-include("gateway.hrl").
-include("player.hrl").

-export([handle/2]).

-spec handle(proto:msg(), #gateway_state{}) -> {[proto:msg()], #gateway_state{}}.
%% 账号登录
handle(#gateway_c_login{account = MsgAccount}, #gateway_state{account = Account} = GateWay) ->
    %% 是否已登录
    ?EC_IF2(Account =/= undefined, ?EC_HAD_LOGIN),
    %% 是否被锁定
    ?EC_NOT_MATCH3(ptolemaios_ll:lock(?LOCAL_LOCK_ACCOUNT1(MsgAccount)), lock, ?EC_OTHER_HAD_LOGIN),
    %% 锁住该账号, 防止并发
    GateWay1 = GateWay#gateway_state{account = MsgAccount},
    Msg = #gateway_s_login{account = MsgAccount, role_list = pkg_role_list(GateWay1)},
    {[Msg], GateWay1};

handle(#gateway_c_create_role{name = Name}, #gateway_state{account = Account} = Gateway) ->
    %% 是否已登录
    ?EC_IF2(Account == undefined, ?EC_NOT_LOGIN),
    %% 数据库是否成功
    ?EC_NOT_MATCH3(vsql:query(?SQL_PLAYER_CREATE2(Account, Name), false),
        ok, ?EC_HAD_REGISTER),
    {[#gateway_s_create_role{role_list = pkg_role_list(Gateway)}], Gateway};

handle(#gateway_c_select_role{id = MsgPlayerId}, #gateway_state{account = Account, player_id = PlayerId} = GateWay) ->
    %% 是否已登录
    ?EC_IF2(Account == undefined, ?EC_NOT_LOGIN),
    ?EC_IF2(PlayerId =/= undefined, ?EC_HAD_LOGIN),
    vsql:ensure_load_ets(player, [MsgPlayerId]),
    %% 角色是否存在
    ?EC_NOT_MATCH3(vsql:lookup_ets(player, [MsgPlayerId]),
        #player{}, ?EC_NO_ROLE),
    %% 先锁定这个id
    case ptolemaios_ll:lock(?LOCAL_LOCK_PLAYER_ID1(MsgPlayerId)) of
        lock ->
            %% 出错保证不死锁
            try
                case player_svr:get_pid(MsgPlayerId) of
                    Pid when is_pid(Pid) ->% 重连
                        ok = exia:call(Pid, ?MSG_PLAYER_GATEWAY_RECONNECT1(self())),
                        erlang:monitor(process, Pid),
                        {[#gateway_s_select_role{id = MsgPlayerId}], GateWay#gateway_state{player_id = MsgPlayerId, player_pid = Pid}};
                    _ ->
                        {ok, Pid} = player_sup:start_child([MsgPlayerId, self()]),
                        erlang:monitor(process, Pid),
                        {[#gateway_s_select_role{id = MsgPlayerId}], GateWay#gateway_state{player_id = MsgPlayerId, player_pid = Pid}}
                end
            catch
                _:_ ->
                    ptolemaios_ll:release(?LOCAL_LOCK_PLAYER_ID1(MsgPlayerId)),
                    ?THROW_EC1(?EC_ERROR)
            end;
        _ ->% 并发
            ?THROW_EC1(?EC_HAD_LOGIN)
    end;

%% 心跳包
handle(#gateway_c_heart{}, Gateway) ->
    {[#gateway_s_heart{}], Gateway};

handle(Msg, Acc) ->
    ?LOG_WARNING("unknow msg ~w", [Msg]),
    {[], Acc}.

pkg_role_list(#gateway_state{account = Account}) ->
    %% 直接操作数据库
    case vsql:query(?SQL_PLAYER_SELECT_ID(Account)) of
        {ok, _, Rows} ->
            lists:map(fun([PlayerId]) ->
                vsql:ensure_load_ets(player, [PlayerId]),
                #player{name = Name} = vsql:lookup_ets(player, [PlayerId]),
                #gateway_p_role_info{id = PlayerId, name = Name}
                      end, Rows);
        _ ->% 失败了
            []
    end.