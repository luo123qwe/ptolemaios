%% @private auto create
-module(gateway_12_handle).

-include("plm_lib.hrl").
-include("game.hrl").
-include("gateway_12_pb.hrl").
-include("gateway.hrl").
-include("player.hrl").

-export([handle/2]).

-spec handle(proto:msg(), #gateway_state{}) -> {[proto:msg()], #gateway_state{}}.
%% 账号登录
handle(#gateway_c_login{account = MsgAccount}, #gateway_state{account = Account} = Gateway) ->
    %% 是否已登录
    ?M11_IF2(Account =/= undefined, ?M11_EC_HAD_LOGIN),
    %% 是否被锁定
    ?M11_NOT_MATCH3(plm_ll:lock(?M13_LL_ACCOUNT1(MsgAccount)), lock, ?M11_EC_OTHER_HAD_LOGIN),
    %% 锁住该账号, 防止并发
    Gateway1 = Gateway#gateway_state{account = MsgAccount},
    Msg = #gateway_s_login{account = MsgAccount, role_list = pkg_role_list(Gateway1)},
    {[Msg], Gateway1};

handle(#gateway_c_create_role{name = Name}, #gateway_state{account = Account} = Gateway) ->
    %% 是否已登录
    ?M11_IF2(Account == undefined, ?M11_EC_NOT_LOGIN),
    %% 数据库是否成功
    ?M11_NOT_MATCH3(plm_sql:query(?M13_SQL_CREATE2(Account, Name), false),
        ok, ?M11_EC_HAD_REGISTER),
    {[#gateway_s_create_role{role_list = pkg_role_list(Gateway)}], Gateway};

handle(#gateway_c_select_role{id = MsgPlayerId}, #gateway_state{account = Account, player_id = PlayerId} = Gateway) ->
    %% 是否已登录
    ?M11_IF2(Account == undefined, ?M11_EC_NOT_LOGIN),
    ?M11_IF2(PlayerId =/= undefined, ?M11_EC_HAD_LOGIN),
    plm_sql:ensure_load_ets(player, [MsgPlayerId]),
    %% 角色是否存在
    ?M11_NOT_MATCH3(plm_sql:lookup_ets(player, [MsgPlayerId]),
        #player{}, ?M11_EC_NO_ROLE),
    %% 先锁定这个id
    case plm_ll:lock(?M13_LL_PLAYER_ID1(MsgPlayerId)) of
        lock ->
            %% 出错保证不死锁
            try
                case player_svr:get_pid(MsgPlayerId) of
                    Pid when is_pid(Pid) ->% 重连
                        ok = plm_svr:call(Pid, ?MSG13_GATEWAY_RECONNECT1(self())),
                        erlang:monitor(process, Pid),
                        {[#gateway_s_select_role{id = MsgPlayerId}], Gateway#gateway_state{player_id = MsgPlayerId, player_pid = Pid}};
                    _ ->
                        {ok, Pid} = player_sup:start_child([MsgPlayerId, self()]),
                        erlang:monitor(process, Pid),
                        {[#gateway_s_select_role{id = MsgPlayerId}], Gateway#gateway_state{player_id = MsgPlayerId, player_pid = Pid}}
                end
            catch
                _:_ ->
                    plm_ll:release(?M13_LL_PLAYER_ID1(MsgPlayerId)),
                    ?M11_THROW_EC1(?M11_EC_ERROR)
            end;
        _ ->% 并发
            ?M11_THROW_EC1(?M11_EC_HAD_LOGIN)
    end;

%% 心跳包
handle(#gateway_c_heart{}, Gateway) ->
    {[#gateway_s_heart{}], Gateway};

handle(Msg, Acc) ->
    ?LOG_WARNING("unknow msg ~w", [Msg]),
    {[], Acc}.

pkg_role_list(#gateway_state{account = Account}) ->
    %% 直接操作数据库
    case plm_sql:query(?M13_SQL_SELECT_ID(Account)) of
        {ok, _, Rows} ->
            lists:map(fun([PlayerId]) ->
                plm_sql:ensure_load_ets(player, [PlayerId]),
                #player{name = Name} = plm_sql:lookup_ets(player, [PlayerId]),
                #gateway_p_role_info{id = PlayerId, name = Name}
                      end, Rows);
        _ ->% 失败了
            []
    end.