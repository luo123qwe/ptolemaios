-module(data_battle_u).

-include("plm_lib.hrl").
-include("battle.hrl").

-export([get/1, get/2]).

get(Key) -> get(Key, true).

get(1, _) -> 
    #data_battle_u{
        id = 1, 
        name = <<"\"普通近战怪物\""/utf8>>, 
        type = [1,102], 
        radius = 1000, 
        skill_arg_1 = [{rate,2},{radius,1000}], 
        skill_arg_2 = [{attack,100}]
    };
get(2, _) -> 
    #data_battle_u{
        id = 2, 
        name = <<"\"普通远程怪物\""/utf8>>, 
        type = [1,103], 
        radius = 5000, 
        skill_arg_1 = [{rate,2},{radius,5000}], 
        skill_arg_2 = [{attack,100}]
    };
get(Key, _) ->
    ?LOG_ERROR("undefined ~w in data_battle_u", [Key]),
    undefined.