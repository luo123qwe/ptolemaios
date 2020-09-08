%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(virture_config).
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
            mysql_poolboy:child_spec(?VMYSQL_POOL, PoolOptions, MySqlOptions),
            #{id => vmysql, start => {vmysql_sup, start_link, []}, type => supervisor}
        ]
    catch
        C:E ->
            ?LOG_WARNING("bad mysql config, ~p", [{C, E}]),
            []
    end.

%% @doc 所有#virture写在这里
-spec all(mysql) -> [#vmysql{}].
all(mysql) ->
    [
        #vmysql{
            table = vmysql_test_player,
            private_key_pos = #vmysql_test_player.vmysql_key,
            state_pos = #vmysql_test_player.vmysql_state,
            select_key = [player_id],
            private_key = [player_id],
            all_fields = [
                #vmysql_field{name = player_id, pos = #vmysql_test_player.player_id, type = ?VIRTURE_UINT32},
                #vmysql_field{name = str, pos = #vmysql_test_player.str, type = ?VIRTURE_STRING},
                #vmysql_field{name = to_str, pos = #vmysql_test_player.to_str, type = ?VIRTURE_TO_STRING},
                #vmysql_field{name = to_bin, pos = #vmysql_test_player.to_bin, type = ?VIRTURE_TO_BINARY},
                #vmysql_field{name = to_json, pos = #vmysql_test_player.to_json, type = ?VIRTURE_JSON_OBJ_LIST([<<"a">>, ?VIRTURE_JSON_OBJ(<<"b">>, [<<"c">>, <<"d">>])])}
            ],
            record_size = record_info(size, vmysql_test_player)
        },
        #vmysql{
            table = vmysql_test_goods,
            private_key_pos = #vmysql_test_goods.vmysql_key,
            state_pos = #vmysql_test_goods.vmysql_state,
            select_key = [player_id],
            private_key = [player_id, goods_id],
            all_fields = [
                #vmysql_field{name = player_id, pos = #vmysql_test_goods.player_id, type = ?VIRTURE_UINT32},
                #vmysql_field{name = goods_id, pos = #vmysql_test_goods.goods_id, type = ?VIRTURE_UINT32},
                #vmysql_field{name = str, pos = #vmysql_test_goods.str, type = ?VIRTURE_STRING},
                #vmysql_field{name = to_str, pos = #vmysql_test_goods.to_str, type = ?VIRTURE_TO_STRING},
                #vmysql_field{name = to_bin, pos = #vmysql_test_goods.to_bin, type = ?VIRTURE_TO_BINARY}
            ],
            index = [[goods_id]],
            record_size = record_info(size, vmysql_test_goods)
        },
        #vmysql{
            table = player,
            private_key_pos = #player.vmysql_key,
            state_pos = #player.vmysql_state,
            select_key = [id],
            private_key = [id],
            all_fields = [
                #vmysql_field{name = id, pos = #player.id, type = ?VIRTURE_UINT64},
                #vmysql_field{name = account, pos = #player.account, type = ?VIRTURE_STRING},
                #vmysql_field{name = name, pos = #player.name, type = ?VIRTURE_STRING}
            ],
            record_size = record_info(size, player)
        }
    ].


%% @doc 获取某个table的定义
-spec get(mysql, atom()) -> #vmysql{}|false.
get(mysql, Table) ->
    lists:keyfind(Table, #vmysql.table, all(mysql)).