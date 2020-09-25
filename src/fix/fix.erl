%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 修复通用模块
%%% @end
%%%-------------------------------------------------------------------
-module(fix).
-author("dominic").

-include("util.hrl").
-include("fix.hrl").

%% API
-export([
    system_init/1,
    fix/1, fix/2
]).

%% 首次执行
-callback fix() -> term().
%% fix()报错, 再次执行
-callback fix_again() -> term().

%% @private 系统初始化
-spec system_init(atom()) -> ok.
system_init(Module) ->
    file:make_dir(?FIX_DETS_PATH),
    {ok, ?DETS_FIX} = dets:open_file(?DETS_FIX, [{file, ?FIX_DETS_PATH ++ "/" ++ atom_to_list(?DETS_FIX)}]),
    case dets:lookup(?DETS_FIX, Module) of
        [{_, _Index, _IsSuccess}] -> skip;
        _ -> system_init(Module, 1)
    end,
    dets:close(?DETS_FIX),
    ok.

%% 新部署的release记录当前最大下标
system_init(Module, Index) ->
    FixModule = ?DYM_FIX_MODULE2(Module, Index),
    case code:load_file(FixModule) of
        {module, _} ->
            system_init(Module, Index + 1);
        _ ->
            dets:insert(?DETS_FIX, {Module, Index - 1, true})
    end.

%% @equiv fix(1)
fix(Module) ->
    fix(Module, 1).

%% @doc 执行重启修复文件```
%% 1, 从上次记录的文件开始修复, 直到没有更多修复文件
%% 2, 没有修复记录, 从默认下标(版本)开始执行修复文件'''
-spec fix(atom(), non_neg_integer()) -> any().
fix(Module, DefaultIndex) ->
    file:make_dir(?FIX_DETS_PATH),
    {ok, ?DETS_FIX} = dets:open_file(?DETS_FIX, [{file, ?FIX_DETS_PATH ++ "/" ++ atom_to_list(?DETS_FIX)}]),
    %% 获取执行下标
    case dets:lookup(?DETS_FIX, Module) of
        [{_, Index0, true}] ->
            Index = Index0 + 1,
            IsFirstTimes = true;
        [{_, Index0, false}] ->
            Index = Index0,
            IsFirstTimes = false;
        _ ->
            Index = DefaultIndex,
            IsFirstTimes = true
    end,
    %% 执行热更代码
    {Index1, IsSuccess} = execute(Module, Index, IsFirstTimes),
    dets:insert(?DETS_FIX, {Module, Index1, IsSuccess}),
    dets:close(?DETS_FIX).

%% 执行代码并且返回最后执行的下标
execute(Module, Index, IsFirstTimes) ->
    FixModule = ?DYM_FIX_MODULE2(Module, Index),
    case code:is_loaded(FixModule) =/= false orelse element(1, code:load_file(FixModule)) =/= error of
        true ->
            IsFail =
                try
                    ?LOG_NOTICE("~w:run()", [FixModule]),
                    ?IF(IsFirstTimes, FixModule:fix(), FixModule:fix_again()),
                    false
                catch
                    C:E:S ->
                        ?LOG_ERROR("restart fix fail fail when ~w~n~w ~w~n~p", [FixModule, C, E, S]),
                        true
                end,
            case IsFail of
                true -> {Index, false};
                _ -> execute(Module, Index + 1, true)
            end;
        false ->
            {Index - 1, true}
    end.