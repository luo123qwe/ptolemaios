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
-export([all/0, get/1]).

%% @doc 所有#virture写在这里
-spec all() -> [#virture{}].
all() ->
    [
        #virture{
            table = player,
            select_key = [id],
            private_key = [id],
            all_fields = [
                #virture_field{name = id, pos = #player.id, type = uint32, default = 0},
                #virture_field{name = account, pos = #player.account, type = string, default = <<>>},
                #virture_field{name = password, pos = #player.password, type = string, default = <<>>}
            ],
            record_size = 4,
            data = []
        }
    ].


%% @doc 获取某个table的定义
get(Table) ->
    lists:keyfind(Table, #virture.table, all()).