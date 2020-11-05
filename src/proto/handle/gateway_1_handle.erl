%% @private auto create
-module(gateway_1_handle).

-include("util.hrl").
-include("error_code.hrl").
-include("gateway_1_pb.hrl").
-include("gateway.hrl").
-include("player.hrl").

-export([handle/2]).

-spec handle(proto:msg(), #gateway{}) -> {[proto:msg()], #gateway{}}.
%% 账号登录
handle(#gateway_c_login{account = MsgAccount}, #gateway{account = Account} = GateWay) ->
    %% 是否已登录
    ?ERROR_CODE_IF2(Account =/= undefined, ?ERROR_CODE_HAD_LOGIN),
    %% 是否被锁定
    ?ERROR_CODE_NOT_MATCH3(local_lock:lock(MsgAccount), lock, ?ERROR_CODE_OTHER_HAD_LOGIN),
    %% 锁住该账号, 防止并发
    GateWay1 = GateWay#gateway{account = MsgAccount},
    Msg = #gateway_s_login{role_list = pkg_role_list(GateWay1)},
    {[Msg], GateWay1};

handle(#gateway_c_create_role{name = Name}, #gateway{account = Account} = Gateway) ->
    %% 是否已登录
    ?ERROR_CODE_IF2(Account == undefined, ?ERROR_CODE_NOT_LOGIN),
    %% 数据库是否成功
    ?ERROR_CODE_NOT_MATCH3(virture_mysql:query(?SQL_PLAYER_CREATE2(Account, Name), false),
        ok, ?ERROR_CODE_HAD_REGISTER),
    {[#gateway_s_create_role{role_list = pkg_role_list(Gateway)}], Gateway};

handle(#gateway_c_select_role{id = MsgPlayerId}, #gateway{account = Account, player_id = PlayerId} = GateWay) ->
    %% 是否已登录
    ?ERROR_CODE_IF2(Account == undefined, ?ERROR_CODE_NOT_LOGIN),
    ?ERROR_CODE_IF2(PlayerId =/= undefined, ?ERROR_CODE_HAD_LOGIN),
    virture_mysql:ensure_load_ets(player, [MsgPlayerId]),
    %% 角色是否存在
    ?ERROR_CODE_NOT_MATCH3(virture_mysql:lookup(player, [MsgPlayerId]),
        #player{}, ?ERROR_CODE_NO_ROLE),
    %% todo 开进程
    {[#gateway_s_select_role{id = MsgPlayerId}], GateWay#gateway{player_id = MsgPlayerId}};

%% 心跳包
handle(#gateway_c_heart{}, Gateway) ->
    {[#gateway_s_heart{}], Gateway};

handle(Msg, Acc) ->
    ?LOG_WARNING("unknow msg ~w", [Msg]),
    {[], Acc}.

pkg_role_list(#gateway{account = Account}) ->
    %% 直接操作数据库
    case virture_mysql:query(?SQL_PLAYER_SELECT_ID(Account)) of
        {ok, _, Rows} ->
            lists:map(fun([PlayerId]) ->
                virture_mysql:load(player, [PlayerId]),
                #player{name = Name} = virture_mysql:lookup(player, [PlayerId]),
                #gateway_p_role_info{id = PlayerId, name = Name}
                      end, Rows);
        _ ->% 失败了
            []
    end.