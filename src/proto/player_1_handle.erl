-module(player_1_handle).

-include("util.hrl").
-include("player_1_pb.hrl").
-include("gateway.hrl").
-include("player.hrl").

-export([handle/2]).

handle(#player_c_account_login{account = Account}, #gateway{socket = Socket} = Gateway) ->
    virture_mysql:init(account, [Account]),
    case virture_mysql:lookup(account, [Account]) of
        undefined ->% 新账号
            IdList = [],
            virture_mysql:insert(#account{account = Account, id_list = []});
        #account{id_list = IdList} ->
            ok
    end,
    
    %% 初始化
    lists:foreach(fun(Id) ->
        virture_mysql:init(player, [Id])
                  end, IdList),
    RoleInfo =
        virture_mysql:fold_cache(fun(_K, #player{id = Id, name = Name}, Acc) ->
            [#player_p_role_info{role_id = Id, name = Name} | Acc]
                                 end, [], account),
    Bin = gateway:pack(#player_s_account_login{role_list = RoleInfo}),
    ranch_tcp:send(Socket, Bin),
    Gateway;
handle(Msg, Acc) ->
    ?LOG_WARNING("unknow msg ~w", [Msg]),
    Acc.

