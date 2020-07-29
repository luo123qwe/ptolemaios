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
                #vmysql_field{name = player_id, pos = #vmysql_test_player.player_id, type = uint32, default = 0},
                #vmysql_field{name = str, pos = #vmysql_test_player.str, type = string, default = 0},
                #vmysql_field{name = to_str, pos = #vmysql_test_player.to_str, type = to_string, default = 0},
                #vmysql_field{name = to_bin, pos = #vmysql_test_player.to_bin, type = to_binary, default = 0}
            ],
            record_size = record_info(size, vmysql_test_player),
            data = ?VIRTURE_LIST,
            sync_size = 1
        },
        #vmysql{
            table = vmysql_test_goods,
            private_key_pos = #vmysql_test_goods.vmysql_key,
            state_pos = #vmysql_test_goods.vmysql_state,
            select_key = [player_id],
            private_key = [player_id, goods_id],
            all_fields = [
                #vmysql_field{name = player_id, pos = #vmysql_test_goods.player_id, type = uint32, default = 0},
                #vmysql_field{name = goods_id, pos = #vmysql_test_goods.goods_id, type = uint32, default = 0},
                #vmysql_field{name = str, pos = #vmysql_test_goods.str, type = string, default = 0},
                #vmysql_field{name = to_str, pos = #vmysql_test_goods.to_str, type = to_string, default = 0},
                #vmysql_field{name = to_bin, pos = #vmysql_test_goods.to_bin, type = to_binary, default = 0}
            ],
            record_size = record_info(size, vmysql_test_goods),
            sync_size = 1
        }
    ].


%% @doc 获取某个table的定义
-spec get(mysql, atom()) -> #vmysql{}|false.
get(mysql, Table) ->
    lists:keyfind(Table, #vmysql.table, all(mysql)).