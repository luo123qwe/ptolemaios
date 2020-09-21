%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 热更新
%%% 顺序执行fix_hot_{index}文件, 并且记录index到dets
%%%
%%% 参考release_handler_1.erl, 提供一些热修复用函数
%%%
%%% 注意挂起进程超低概率(甚至没有在实际中遇到过)引起并发问题, 因为挂起进程是先拿到所有进程再挂起,
%%% 读和挂起中间的时间可能会产生新的进程, 而这些新的进程将不会被挂起和执行相关代码,
%%% 并且会被kill掉(lib/sasl-3.5/src/release_handler_1.erl:369)
%%%
%%% 注意数据是不可回滚的, 代码报错时, 再次执行修复会重复执行一次代码,
%%% 所以通过get/set在新代码打上标识再次执行, 跳过已经正确执行的代码
%%% @end
%%%-------------------------------------------------------------------
-module(fix_hot).
-author("dominic").

-include("util.hrl").
-include("fix.hrl").

%% API
-export([
    system_init/0,
    fix/0, fix/1,
    get/3, set/3,
    suspend/1, resume/1,
    reload_shell/0, reload_shell/2,
    reload_release/0, reload_release/1
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
    Module = ?FIX_HOT_MODULE(Index),
    case code:load_file(Module) of
        {module, _} ->
            system_init(Index + 1);
        _ ->
            dets:insert(?DETS_FIX, {?MODULE, Index - 1})
    end.

%% @equiv fix(1)
fix() ->
    fix(1).

%% @doc 执行热修复文件```
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
    Module = ?FIX_HOT_MODULE(Index),
    case code:is_loaded(Module) =/= false orelse element(1, code:load_file(Module)) =/= error of
        true ->
            IsFail =
                try
                    ?LOG_NOTICE("~w:run()", [Module]),
                    Module:run(),
                    false
                catch
                    C:E:S ->
                        ?LOG_ERROR("hot fix fail fail when ~w~n~w ~w~n~p", [Module, C, E, S]),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 参考release_handler_1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc 挂起一个监督者(包括它本身)下的所有进程
-spec suspend([supervisor:sup_ref()]) -> [pid()].
suspend(SupList) ->
    suspend_sup(SupList, []).

suspend_sup([], SuspendList) ->
    SuspendList;
suspend_sup([Sup | T], SuspendList) ->
    case catch supervisor:which_children(Sup) of
        ChildList when is_list(ChildList) ->
            SuspendList1 = suspend_pid(Sup, SuspendList),
            case suspend_child(ChildList, []) of
                NewSuspendList when is_list(NewSuspendList) ->
                    suspend_sup(T, NewSuspendList ++ SuspendList1);
                _Error ->
                    resume(lists:reverse(SuspendList1)),
                    error(suspend_fail)
            end;
        Error ->
            ?LOG_ERROR("~p", [Error]),
            resume(lists:reverse(SuspendList)),
            error(suspend_fail)
    end.

suspend_child([], SuspendList) ->
    SuspendList;
suspend_child([{_Name, Pid, worker, _Mods} | T], SuspendList) when is_pid(Pid) ->
    SuspendList1 = suspend_pid(Pid, SuspendList),
    suspend_child(T, SuspendList1);
suspend_child([{Name, Pid, supervisor, _Mods} | T], SuspendList) when is_pid(Pid) ->
    case catch supervisor:which_children(Pid) of
        ChildList when is_list(ChildList) ->
            SuspendList1 = suspend_pid(Pid, SuspendList),
            case catch suspend_child(ChildList, []) of
                NewSuspendList when is_list(NewSuspendList) ->
                    %% 先暂停的先恢复
                    suspend_child(T, NewSuspendList ++ SuspendList1);
                _Error ->
                    resume(lists:reverse(SuspendList)),
                    error(suspend_fail)
            end;
        Other ->
            ?LOG_ERROR("release_handler: ~p~nerror during"
            " a which_children call to ~p (~w)."
            " [State: running] Exiting ... ~n",
                [Other, Name, Pid]),
            error(which_children_failed)
    end;
suspend_child([_ | T], SuspendList) ->
    suspend_child(T, SuspendList).

suspend_pid(Pid, SuspendList) ->
    case catch sys:suspend(Pid) of
        ok ->
            [Pid | SuspendList];
        Error ->
            ?LOG_ERROR("~p", [Error]),
            resume(lists:reverse(SuspendList)),
            error(suspend_fail)
    end.

%% @doc 恢复进程, 配合suspend/1使用
-spec resume([pid()]) -> ok.
resume([]) ->
    ok;
resume([H | T]) ->
    case catch sys:resume(H) of
        ok -> ok;
        Error ->
            ?LOG_ERROR("~p", [Error])
    end,
    resume([H | T]).

%% @equiv  reload_shell(default, [ptolemaios])
reload_shell() ->
    reload_shell(default, [ptolemaios]).

%% @doc 对于rebar3 shell启动的终端, 重载app的代码
-spec reload_shell(atom(), [atom()]) -> any().
reload_shell(Profile, AppList) ->
    ProfileStr = atom_to_list(Profile),
    %% 先编译
    ?LOG_NOTICE("~s", [string:replace(os:cmd("cd . && \"./rebar3\" as " ++ ProfileStr ++ " compile"), "\n", "~n")]),
    %% 获取更新的模块
    Modules =
        lists:foldl(fun(App, Acc1) ->
            %% 代码路径
            EBinDir = "./_build/" ++ ProfileStr ++ "/lib/" ++ atom_to_list(App) ++ "/ebin",
            get_app_modules(EBinDir) ++ Acc1
                    end, [], AppList),
    {ok, Prepared} = code:prepare_loading(Modules),
    [code:purge(M) || M <- Modules],
    code:finish_loading(Prepared),
    ?LOG_NOTICE("load: ~w", [Modules]).

%% @equiv  reload_release([ptolemaios])
reload_release() ->
    reload_release([ptolemaios]).

%% @doc 对于release启动的终端, 重载app的代码
-spec reload_release([atom()]) -> any().
reload_release(AppList) ->
    {ok, LibDirList} = file:list_dir("./lib"),
    %% 获取更新的模块
    Modules =
        lists:foldl(fun(App, Acc1) ->
            %% 找到代码路径
            LibDir = get_lib_dir(atom_to_list(App), LibDirList),
            EBinDir = "./lib/" ++ LibDir ++ "/ebin",
            get_app_modules(EBinDir) ++ Acc1
                    end, [], AppList),
    {ok, Prepared} = code:prepare_loading(Modules),
    [code:purge(M) || M <- Modules],
    code:finish_loading(Prepared),
    ?LOG_NOTICE("load: ~w", [Modules]).

get_app_modules(EBinDir) ->
    %% 拿到所有模块, 对比版本
    filelib:fold_files(EBinDir, ".beam", true, fun(File, Acc2) ->
        Module = list_to_atom(filename:rootname(filename:basename(File))),
        %% 获取代码版本
        OldVsn = get_current_vsn(Module),
        NewVsn = get_vsn(File),
        case OldVsn == unload orelse OldVsn == NewVsn of
            true -> Acc2;
            false -> [Module | Acc2]
        end
                                               end,
        []).

get_current_vsn(Mod) ->
    case code:is_loaded(Mod) of
        {file, _} ->
            Attributes = Mod:module_info(attributes),
            case lists:keyfind(vsn, 1, Attributes) of
                {vsn, [Vsn]} ->
                    Vsn;
                error ->
                    undefined
            end;
        _ ->
            unload
    end.

%%-----------------------------------------------------------------
%% Func: get_vsn/1
%% Args: Bin = binary()
%% Purpose: Finds the version attribute of a module.
%% Returns: Vsn = term()
%%-----------------------------------------------------------------
get_vsn(Bin) ->
    {ok, {_Mod, Vsn}} = beam_lib:version(Bin),
    case misc_supp:is_string(Vsn) of
        true ->
            Vsn;
        false ->
            %% If -vsn(Vsn) defines a term which is not a
            %% string, the value is returned here as [Vsn].
            case Vsn of
                [VsnTerm] ->
                    VsnTerm;
                _ ->
                    Vsn
            end
    end.

get_lib_dir(AppStr, LibDirList) ->
    AppStr1 = AppStr ++ "-",
    [AppDir]
        = lists:filter(fun(LibDir) ->
        case re:run(LibDir, AppStr1) of
            {match, _} -> true;
            _ -> false
        end
                       end, LibDirList),
    AppDir.