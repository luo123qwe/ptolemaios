%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(virture_config).
-author("dominic").

-include("virture.hrl").
-include("player.hrl").

%% API
-export([all/1, get/2, get_sup_spec/1]).

%% @doc 获取sup范式
-spec get_sup_spec(mysql) -> [supervisor:child_spec()].
get_sup_spec(mysql) ->
    [#{id => virture_mysql, start => {virture_mysql_sup, start_link, []}, type => supervisor}].

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
        }
    ].


%% @doc 获取某个table的定义
-spec get(mysql, atom()) -> #vmysql{}|false.
get(mysql, Table) ->
    lists:keyfind(Table, #vmysql.table, all(mysql)).