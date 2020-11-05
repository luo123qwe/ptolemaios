%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc virture配置
%%%
%%% 配置用erl文件写可以使用idea的提示, 比写config里面方便太多了
%%% @end
%%%-------------------------------------------------------------------
-module(virture).
-author("dominic").

-include("util.hrl").
-include("virture.hrl").
-include("player.hrl").

%% API
-export([all/1, get/2, get_sup_spec/0]).

%% @doc 获取sup范式
-spec get_sup_spec() -> [supervisor:child_spec()].
get_sup_spec() ->
    {ok, Config} = application:get_env(ptolemaios, virture),
    case lists:keyfind(type, 1, Config) of
        {_, VirtureType} ->
            get_sup_spec(VirtureType);
        _ ->
            ?LOG_WARNING("no virture type"),
            []
    end.

get_sup_spec(mysql) ->
    try
        {ok, Config} = application:get_env(ptolemaios, virture),
        {_, User} = lists:keyfind(mysql_user, 1, Config),
        {_, Password} = lists:keyfind(mysql_password, 1, Config),
        {_, Database} = lists:keyfind(mysql_database, 1, Config),
        PoolOptions = [{size, 50}, {max_overflow, 100}],
        MySqlOptions = [{user, User}, {password, Password}, {database, Database},
            {keep_alive, true},
            {prepare, []}],%{test, "SELECT * FROM player WHERE id=?"}]}],
        [
            mysql_poolboy:child_spec(?VIRTURE_MYSQL_POOL, PoolOptions, MySqlOptions),
            #{id => virture_mysql_sup, start => {virture_mysql_sup, start_link, []}, type => supervisor}
        ]
    catch
        C:E ->
            ?LOG_WARNING("bad mysql config, ~p", [{C, E}]),
            []
    end.

%% @doc 所有表定义写在这里
-spec all(mysql) -> [#virture_mysql{}].
all(mysql) ->
    [
        #virture_mysql{
            table = virture_mysql_test_player,
            private_key_pos = #virture_mysql_test_player.virture_mysql_key,
            state_pos = #virture_mysql_test_player.virture_mysql_state,
            select_key = [player_id],
            private_key = [player_id],
            all_fields = [
                #virture_mysql_field{name = player_id, pos = #virture_mysql_test_player.player_id, type = ?VIRTURE_UINT32},
                #virture_mysql_field{name = str, pos = #virture_mysql_test_player.str, type = ?VIRTURE_STRING},
                #virture_mysql_field{name = to_str, pos = #virture_mysql_test_player.to_str, type = ?VIRTURE_TO_STRING},
                #virture_mysql_field{name = to_bin, pos = #virture_mysql_test_player.to_bin, type = ?VIRTURE_TO_BINARY},
                #virture_mysql_field{name = to_json, pos = #virture_mysql_test_player.to_json, type = ?VIRTURE_JSON_OBJ_LIST([<<"a">>, ?VIRTURE_JSON_OBJ(<<"b">>, [<<"c">>, <<"d">>])])}
            ],
            record_size = record_info(size, virture_mysql_test_player)
        },
        #virture_mysql{
            table = virture_mysql_test_goods,
            private_key_pos = #virture_mysql_test_goods.virture_mysql_key,
            state_pos = #virture_mysql_test_goods.virture_mysql_state,
            select_key = [player_id],
            private_key = [player_id, goods_id],
            all_fields = [
                #virture_mysql_field{name = player_id, pos = #virture_mysql_test_goods.player_id, type = ?VIRTURE_UINT32},
                #virture_mysql_field{name = goods_id, pos = #virture_mysql_test_goods.goods_id, type = ?VIRTURE_UINT32},
                #virture_mysql_field{name = str, pos = #virture_mysql_test_goods.str, type = ?VIRTURE_STRING},
                #virture_mysql_field{name = to_str, pos = #virture_mysql_test_goods.to_str, type = ?VIRTURE_TO_STRING},
                #virture_mysql_field{name = to_bin, pos = #virture_mysql_test_goods.to_bin, type = ?VIRTURE_TO_BINARY}
            ],
            index = [[goods_id]],
            record_size = record_info(size, virture_mysql_test_goods)
        },
        #virture_mysql{
            table = virture_mysql_test_equip,
            use_ets = false,
            private_key_pos = #virture_mysql_test_equip.virture_mysql_key,
            state_pos = #virture_mysql_test_equip.virture_mysql_state,
            select_key = [player_id],
            private_key = [player_id, equip_id],
            all_fields = [
                #virture_mysql_field{name = player_id, pos = #virture_mysql_test_equip.player_id, type = ?VIRTURE_UINT32},
                #virture_mysql_field{name = equip_id, pos = #virture_mysql_test_equip.equip_id, type = ?VIRTURE_UINT32}
            ],
            record_size = record_info(size, virture_mysql_test_equip)
        },
        #virture_mysql{
            table = player,
            private_key_pos = #player.virture_mysql_key,
            state_pos = #player.virture_mysql_state,
            select_key = [id],
            private_key = [id],
            all_fields = [
                #virture_mysql_field{name = id, pos = #player.id, type = ?VIRTURE_UINT64},
                #virture_mysql_field{name = account, pos = #player.account, type = ?VIRTURE_STRING},
                #virture_mysql_field{name = name, pos = #player.name, type = ?VIRTURE_STRING}
            ],
            record_size = record_info(size, player)
        }
    ].


%% @doc 获取某个table的定义
-spec get(mysql, atom()) -> #virture_mysql{}|false.
get(mysql, Table) ->
    lists:keyfind(Table, #virture_mysql.table, all(mysql)).