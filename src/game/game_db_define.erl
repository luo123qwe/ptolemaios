%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 数据库定义
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(game_db_define).
-author("dominic").

-include("plm_lib.hrl").
-include("player.hrl").

%% API
-export([get_default_db/0, get/0, get/1]).

%% @doc 获取默认数据库
-spec get_default_db() -> atom().
get_default_db() ->
    %% 在配置里面拿到游戏数据库
    {ok, ConfigList} = application:get_env(ptolemaios, plm_db),
    {_, DBConfigList} = lists:keyfind(db_list, 1, ConfigList),
    GameDB =
        lists:foldl(fun(DBConfig, Acc) ->
            case lists:keyfind(type, 1, DBConfig) of
                {_, game} -> list_to_atom(plm_kv:lookup([mysql_database], DBConfig, undefined));
                _ -> Acc
            end
                    end, false, DBConfigList),
    ?DO_IF(GameDB == false, exit(badarg)),
    GameDB.

%% @doc 获取数据表定义
-spec get() -> [#plm_sql{}].
get() ->
    GameDB = get_default_db(),
    game_db_define:get(GameDB).

%% @doc 获取数据表定义
-spec get(atom()) -> [#plm_sql{}].
get(GameDB) ->
    [
        #plm_sql{
            db = GameDB,
            table = player,
            private_key_pos = #player.plm_sql_key,
            state_pos = #player.plm_sql_state,
            select_key = [id],
            private_key = [id],
            all_fields = [
                #plm_sql_field{name = id, pos = #player.id, type = ?PLM_DB_UINT64, auto_incremental = 1},
                #plm_sql_field{name = account, pos = #player.account, type = ?PLM_DB_STRING1(100)},
                #plm_sql_field{name = name, pos = #player.name, type = ?PLM_DB_STRING1(100)}
            ],
            index = [[account]],
            unique_index = [[name]],
            record_size = record_info(size, player)
        }
    ].
