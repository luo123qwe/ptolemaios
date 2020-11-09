-module(data_dynames_unit).

-include("dynames.hrl").

-export([get/1]).

get(1) -> 
    #data_dynames_unit{id = 1, name = <<"\"普通近战怪物\""/utf8>>, type = [1,102], radius = 1000, skill_arg_1 = [{rate,2},{radius,1000}], skill_arg_2 = [{attack,100}]};
get(2) -> 
    #data_dynames_unit{id = 2, name = <<"\"普通远程怪物\""/utf8>>, type = [1,103], radius = 5000, skill_arg_1 = [{rate,2},{radius,5000}], skill_arg_2 = [{attack,100}]};
get(_) -> undefined.