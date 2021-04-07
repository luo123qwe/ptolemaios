%%%-------------------------------------------------------------------
%%% @author Dominic
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 重启修复模板, 热更模板降维打击, 详细可以参考热更模板
%%%
%%% @end
%%% Created : 30. 3月 2021 16:12
%%%-------------------------------------------------------------------
-module(template_fix_restart).
-author("Dominic").

-behaviour(plm_fix).

-include("plm_lib.hrl").

%% API
-export([fix/0, fix_again/0]).

-record(template_fix_restart, {a, b, c}).

%% 修复
fix() ->
    FixTable = template_fix_restart,
    %% 修复dets数据
    DetsFixFun = fun({delete, ThisTable, Key}) when ThisTable == FixTable ->
        plm_sql:delete(FixTable, Key);
        (#template_fix_restart{} = Record) ->
            plm_sql:insert(Record);
        (NotExcept) ->
            exit({not_except, NotExcept})
                 end,
    plm_sql:fix_dets(undefined, DetsFixFun, undefined),
    %% 修复数据库
    plm_sql:query("update template_fix_restart set a=1 where a=2").

%% 第一次修复失败时(可能调用了fix(), 也可能调用了用fix_again()), 再次进行修复调用这个
fix_again() ->
    fix().