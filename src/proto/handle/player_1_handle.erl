-module(player_1_handle).

-include("util.hrl").
-include("player_1_pb.hrl").
-include("gateway.hrl").
-include("player.hrl").

-export([handle/2]).

handle(#player_c_account_login{account = Account}, #gateway{socket = Socket} = Gateway) ->
    %% 防止数据混淆
    virture_mysql:clean_pd(),
    virture_mysql:load(player, undefined, ["account=", $', Account, $']),
    
    RoleInfo =
        virture_mysql:fold_cache(fun(_K, #player{id = Id, name = Name}, Acc) ->
            [#player_p_role_info{id = Id, name = Name} | Acc]
                                 end, [], player),
    Bin = gateway_server:pack(#player_s_account_login{role_list = RoleInfo}),
    ?LOG_DEBUG("~p", [Bin]),
    ranch_tcp:send(Socket, Bin),
    Gateway#gateway{account = Account};

handle(#player_c_select_role{id = Id}, #gateway{socket = Socket} = Gateway) ->
    case virture_mysql:lookup(player, [Id]) of
        #player{} ->
            %% todo 两个进程互相监督
            {ok, Pid} = player_sup:start_child(Id),
            Bin = gateway_server:pack(#player_s_select_role{id = Id}),
            ranch_tcp:send(Socket, Bin),
            Gateway#gateway{player_id = Id, player_pid = Pid};
        _ ->
            Gateway
    end;

handle(#player_c_create_role{name = _Name}, #gateway{socket = Socket} = Gateway) ->
    %% todo 晚点再做
    RoleInfo =
        virture_mysql:fold_cache(fun(_K, #player{id = Id, name = Name}, Acc) ->
            [#player_p_role_info{id = Id, name = Name} | Acc]
                                 end, [], player),
    Bin = gateway_server:pack(#player_s_create_role{role_list = RoleInfo}),
    ranch_tcp:send(Socket, Bin),
    Gateway;

handle(Msg, Acc) ->
    ?LOG_WARNING("unknow msg ~w", [Msg]),
    Acc.

