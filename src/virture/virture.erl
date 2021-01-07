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
-include("gateway.hrl").
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
            mysql_poolboy:child_spec(?VSQL_POOL, PoolOptions, MySqlOptions),
            #{id => vsql_sup, start => {vsql_sup, start_link, []}, type => supervisor}
        ]
    catch
        C:E ->
            ?LOG_WARNING("bad mysql config, ~p", [{C, E}]),
            []
    end.

%% @doc 所有表定义写在这里
-spec all(mysql) -> [#vsql{}].
all(mysql) ->
    [
        #vsql{
            table = vsql_test_player,
            private_key_pos = #vsql_test_player.vsql_key,
            state_pos = #vsql_test_player.vsql_state,
            select_key = [player_id],
            private_key = [player_id],
            all_fields = [
                #vsql_field{name = player_id, pos = #vsql_test_player.player_id, type = ?VIRTURE_UINT32},
                #vsql_field{name = str, pos = #vsql_test_player.str, type = ?VIRTURE_STRING1(100)},
                #vsql_field{name = to_str, pos = #vsql_test_player.to_str, type = ?VIRTURE_TO_STRING},
                #vsql_field{name = to_bin, pos = #vsql_test_player.to_bin, type = ?VIRTURE_TO_BINARY},
                #vsql_field{name = to_json, pos = #vsql_test_player.to_json, type = ?VIRTURE_JSON_OBJ_LIST([<<"a">>, ?VIRTURE_JSON_OBJ(<<"b">>, [<<"c">>, <<"d">>])])}
            ],
            record_size = record_info(size, vsql_test_player)
        },
        #vsql{
            table = vsql_test_goods,
            private_key_pos = #vsql_test_goods.vsql_key,
            state_pos = #vsql_test_goods.vsql_state,
            select_key = [player_id],
            private_key = [player_id, goods_id],
            all_fields = [
                #vsql_field{name = player_id, pos = #vsql_test_goods.player_id, type = ?VIRTURE_UINT32},
                #vsql_field{name = goods_id, pos = #vsql_test_goods.goods_id, type = ?VIRTURE_UINT32},
                #vsql_field{name = str, pos = #vsql_test_goods.str, type = ?VIRTURE_STRING1(100)},
                #vsql_field{name = to_str, pos = #vsql_test_goods.to_str, type = ?VIRTURE_TO_STRING},
                #vsql_field{name = to_bin, pos = #vsql_test_goods.to_bin, type = ?VIRTURE_TO_BINARY}
            ],
            index = [[goods_id]],
            record_size = record_info(size, vsql_test_goods)
        },
        #vsql{
            table = vsql_test_equip,
            use_ets = false,
            private_key_pos = #vsql_test_equip.vsql_key,
            state_pos = #vsql_test_equip.vsql_state,
            select_key = [player_id],
            private_key = [player_id, equip_id],
            all_fields = [
                #vsql_field{name = player_id, pos = #vsql_test_equip.player_id, type = ?VIRTURE_UINT32},
                #vsql_field{name = equip_id, pos = #vsql_test_equip.equip_id, type = ?VIRTURE_UINT32}
            ],
            record_size = record_info(size, vsql_test_equip)
        },
        #vsql{
            table = player,
            private_key_pos = #player.vsql_key,
            state_pos = #player.vsql_state,
            select_key = [id],
            private_key = [id],
            all_fields = [
                #vsql_field{name = id, pos = #player.id, type = ?VIRTURE_UINT64, auto_incremental = 1},
                #vsql_field{name = account, pos = #player.account, type = ?VIRTURE_STRING1(100)},
                #vsql_field{name = name, pos = #player.name, type = ?VIRTURE_STRING1(100)}
            ],
            index = [[account]],
            unique_index = [[name]],
            record_size = record_info(size, player)
        }
    ].


%% @doc 获取某个table的定义
-spec get(mysql, atom()) -> #vsql{}|false.
get(mysql, Table) ->
    lists:keyfind(Table, #vsql.table, all(mysql)).