%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 重启更新
%%% 顺序执行fix_restart_{index}文件, 并且记录index到dets
%%%
%%% 注意数据是不可回滚的, 若执行到某个函数失败时, 有这两种情况```
%%% 第一, 已经执行过该修复模块, 修改对应的tar的模块, 再次进行更新, 这里的修改都是针对某些已经部署的release(不同环境下可能会有不同的修复), 因为新的release会直接执行正确的代码
%%% 第二, 未经执行过该修复模块, 转情况一'''
%%% @end
%%%-------------------------------------------------------------------
-module(fix_restart).
-author("dominic").

-include("util.hrl").
-include("fix.hrl").

%% API
-export([
    system_init/0,
    fix/0, fix/1
]).

%% @private 系统初始化
system_init() ->
    fix:system_init(?MODULE).

%% @equiv fix(1)
fix() ->
    fix:fix(?MODULE).

%% @doc 执行重启修复文件```
%% 1, 从上次记录的文件开始修复, 直到没有更多修复文件
%% 2, 没有修复记录, 从默认下标(版本)开始执行修复文件'''
fix(DefaultIndex) ->
    fix:fix(?MODULE, DefaultIndex).


