%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 重启更新
%%% 顺序执行fix_restart_{index}文件, 并且记录index到dets
%%%
%%% 注意数据是不可回滚的, 代码报错时, 再次重启会重复执行一次代码,
%%% 所以通过get/set在新代码打上标识再次执行, 跳过已经正确执行的代码
%%% @end
%%%-------------------------------------------------------------------
-module(fix_restart).
-author("dominic").

-include("util.hrl").
-include("fix.hrl").

%% API
-export([
    system_init/0,
    fix/0, fix/1,
    set/3, get/3
]).

-callback run() -> term().

%% @private 系统初始化
system_init() ->
    file:make_dir(?FIX_DETS_PATH),
    {ok, ?DETS_FIX} = dets:open_file(?DETS_FIX, [{file, ?FIX_DETS_PATH ++ "/" ++ atom_to_list(?DETS_FIX)}]),
    case dets:lookup(?DETS_FIX, ?MODULE) of
        [{_, _Index}] -> skip;
        _ -> system_init(1)
    end,
    dets:close(?DETS_FIX),
    ok.

system_init(Index) ->
    Module = ?FIX_RESTART_MODULE(Index),
    case code:load_file(Module) of
        {module, _} ->
            system_init(Index + 1);
        _ ->
            dets:insert(?DETS_FIX, {?MODULE, Index - 1})
    end.

%% @equiv fix(1)
fix() ->
    fix(1).

%% @doc 执行重启修复文件```
%% 1, 从上次记录的文件开始修复, 直到没有更多修复文件
%% 2, 没有修复记录, 从默认下标(版本)开始执行修复文件'''
fix(DefaultIndex) ->
    file:make_dir(?FIX_DETS_PATH),
    {ok, ?DETS_FIX} = dets:open_file(?DETS_FIX, [{file, ?FIX_DETS_PATH ++ "/" ++ atom_to_list(?DETS_FIX)}]),
    %% 获取执行下标
    case dets:lookup(?DETS_FIX, ?MODULE) of
        [{_, Index0}] -> Index = Index0 + 1;
        _ -> Index = DefaultIndex
    end,
    %% 执行热更代码
    case Index of
        undefined -> ok;
        _ -> dets:insert(?DETS_FIX, {?MODULE, execute(Index)})
    end,
    dets:close(?DETS_FIX).

%% 执行代码并且返回最后执行的下标
execute(Index) ->
    Module = ?FIX_RESTART_MODULE(Index),
    case code:is_loaded(Module) =/= false orelse element(1, code:load_file(Module)) =/= error of
        true ->
            IsFail =
                try
                    ?LOG_NOTICE("~w:run()", [Module]),
                    Module:run(),
                    false
                catch
                    C:E:S ->
                        ?LOG_ERROR("restart fix fail fail when ~w~n~w ~w~n~p", [Module, C, E, S]),
                        true
                end,
            case IsFail of
                true -> Index;
                _ -> execute(Index + 1)
            end;
        false ->
            Index - 1
    end.

%% @doc 设置kv
-spec set(any(), any(), any()) -> ok.
set(Index, Key, Value) ->
    dets:insert(?DETS_FIX, {{?MODULE, Index, Key}, Value}),
    ok.

%% @doc 获取kv
-spec set(any(), any(), any()) -> ok.
get(Index, Key, Default) ->
    case dets:lookup(?DETS_FIX, {?MODULE, Index, Key}) of
        [{_, Value}] ->
            Value;
        Default ->
            Default
    end.


