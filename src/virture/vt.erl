%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc virture配置
%%%
%%% 配置用erl文件写可以使用idea的提示, 比写config里面方便太多了
%%% @end
%%%-------------------------------------------------------------------
-module(vt).
-author("dominic").

-include("util.hrl").
-include("vt.hrl").
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
            mysql_poolboy:child_spec(?VT_SQL_POOL, PoolOptions, MySqlOptions),
            #{id => vt_sql_sup, start => {vt_sql_sup, start_link, []}, type => supervisor}
        ]
    catch
        C:E ->
            ?LOG_WARNING("bad mysql config, ~p", [{C, E}]),
            []
    end.

%% @doc 所有表定义写在这里
-spec all(mysql) -> [#vt_sql{}].
all(mysql) ->
    [
        #vt_sql{
            table = vt_sql_test_player,
            private_key_pos = #vt_sql_test_player.vt_sql_key,
            state_pos = #vt_sql_test_player.vt_sql_state,
            select_key = [player_id],
            private_key = [player_id],
            all_fields = [
                #vt_sql_field{name = player_id, pos = #vt_sql_test_player.player_id, type = ?VT_UINT32},
                #vt_sql_field{name = str, pos = #vt_sql_test_player.str, type = ?VT_STRING},
                #vt_sql_field{name = to_str, pos = #vt_sql_test_player.to_str, type = ?VT_TO_STRING},
                #vt_sql_field{name = to_bin, pos = #vt_sql_test_player.to_bin, type = ?VT_TO_BINARY},
                #vt_sql_field{name = to_json, pos = #vt_sql_test_player.to_json, type = ?VT_JSON_OBJ_LIST([<<"a">>, ?VT_JSON_OBJ(<<"b">>, [<<"c">>, <<"d">>])])}
            ],
            record_size = record_info(size, vt_sql_test_player)
        },
        #vt_sql{
            table = vt_sql_test_goods,
            private_key_pos = #vt_sql_test_goods.vt_sql_key,
            state_pos = #vt_sql_test_goods.vt_sql_state,
            select_key = [player_id],
            private_key = [player_id, goods_id],
            all_fields = [
                #vt_sql_field{name = player_id, pos = #vt_sql_test_goods.player_id, type = ?VT_UINT32},
                #vt_sql_field{name = goods_id, pos = #vt_sql_test_goods.goods_id, type = ?VT_UINT32},
                #vt_sql_field{name = str, pos = #vt_sql_test_goods.str, type = ?VT_STRING},
                #vt_sql_field{name = to_str, pos = #vt_sql_test_goods.to_str, type = ?VT_TO_STRING},
                #vt_sql_field{name = to_bin, pos = #vt_sql_test_goods.to_bin, type = ?VT_TO_BINARY}
            ],
            index = [[goods_id]],
            record_size = record_info(size, vt_sql_test_goods)
        },
        #vt_sql{
            table = vt_sql_test_equip,
            use_ets = false,
            private_key_pos = #vt_sql_test_equip.vt_sql_key,
            state_pos = #vt_sql_test_equip.vt_sql_state,
            select_key = [player_id],
            private_key = [player_id, equip_id],
            all_fields = [
                #vt_sql_field{name = player_id, pos = #vt_sql_test_equip.player_id, type = ?VT_UINT32},
                #vt_sql_field{name = equip_id, pos = #vt_sql_test_equip.equip_id, type = ?VT_UINT32}
            ],
            record_size = record_info(size, vt_sql_test_equip)
        },
        #vt_sql{
            table = player,
            private_key_pos = #player.vt_sql_key,
            state_pos = #player.vt_sql_state,
            select_key = [id],
            private_key = [id],
            all_fields = [
                #vt_sql_field{name = id, pos = #player.id, type = ?VT_UINT64},
                #vt_sql_field{name = account, pos = #player.account, type = ?VT_STRING},
                #vt_sql_field{name = name, pos = #player.name, type = ?VT_STRING}
            ],
            record_size = record_info(size, player)
        }
    ].


%% @doc 获取某个table的定义
-spec get(mysql, atom()) -> #vt_sql{}|false.
get(mysql, Table) ->
    lists:keyfind(Table, #vt_sql.table, all(mysql)).