%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ptolemaios_virture_define).
-author("dominic").

-include("virture.hrl").
-include("player.hrl").

%% API
-export([get/0]).

get() ->
    [
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
