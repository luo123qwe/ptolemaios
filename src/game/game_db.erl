%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 数据库定义
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(game_db).
-author("dominic").

-include("plm_lib.hrl").
-include("player.hrl").

-compile({no_auto_import, [get/0]}).

%% API
-export([get_plm_sql_cfg/1]).

%% @doc 获取数据表定义
-spec get_plm_sql_cfg(atom()) -> [#plm_sql{}].
get_plm_sql_cfg(GameDB) ->
    [
        #plm_sql_cfg{
            db = GameDB,
            record = #player{
                id = #plm_sql_field{type = ?PLM_DB_UINT64, key_type = ?PLM_SQL_KEY_TYPE_SELECT, ai = 1},
                name = #plm_sql_field{type = ?PLM_DB_STRING1(100)},
                account = #plm_sql_field{type = ?PLM_DB_STRING1(100)}
            },
            record_fields = record_info(fields, player),
            index = [[#player.account]],
            unique_index = [[#player.name]]
        }
    ].
