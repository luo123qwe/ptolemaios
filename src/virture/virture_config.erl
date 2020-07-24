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
-spec all(mysql) -> [#virture_mysql{}].
all(mysql) ->
    Base = #virture_mysql{pool = virture_mysql},
    [
        Base#virture_mysql{
            table = player,
            select_key = [id],
            private_key = [id],
            all_fields = [
                #virture_mysql_field{name = id, pos = #player.id, type = uint32, default = 0},
                #virture_mysql_field{name = account, pos = #player.account, type = string, default = <<>>},
                #virture_mysql_field{name = password, pos = #player.password, type = string, default = <<>>}
            ],
            record_size = 4,
            data = []
        }
    ].


%% @doc 获取某个table的定义
-spec get(mysql, atom()) -> #virture_mysql{}|false.
get(mysql, Table) ->
    lists:keyfind(Table, #virture_mysql.table, all(mysql)).