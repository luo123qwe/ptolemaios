-module(data_dynames_unit).

-include("dynames.hrl").

-export([get/1]).

get(<<"1"/utf8>>) -> 
    #data_dynames_unit{id = <<"1"/utf8>>, name = <<"\"普通近战怪物\""/utf8>>, type = <<"[1,102]"/utf8>>, radius = <<"1000"/utf8>>, skill_arg_1 = <<"{\"radius\":1000,\"rate\":2}"/utf8>>, skill_arg_2 = <<"{\"attack\":100}"/utf8>>};
get(<<"2"/utf8>>) -> 
    #data_dynames_unit{id = <<"2"/utf8>>, name = <<"\"普通远程怪物\""/utf8>>, type = <<"[1,103]"/utf8>>, radius = <<"5000"/utf8>>, skill_arg_1 = <<"{\"radius\":5000,\"rate\":2}"/utf8>>, skill_arg_2 = <<"{\"attack\":100}"/utf8>>};
get(_) -> undefined.