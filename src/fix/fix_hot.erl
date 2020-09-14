%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 热更新
%%% @end
%%%-------------------------------------------------------------------
-module(fix_hot).
-author("dominic").

-include("util.hrl").

%% API
-export([fix/0, suspend/1, resume/1]).

fix() ->
    ?LOG_ERROR.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 参考release_handler_1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

resume([]) ->
    ok;
resume([H | T]) ->
    case catch sys:resume(H) of
        ok -> ok;
        Error ->
            ?LOG_ERROR("~p", [Error])
    end,
    resume([H | T]).

%%reload() ->
%%    SearchPathList =
%%        lists:foldl(fun(CodePath, Acc) ->
%%
%%                    end, code:get_path()),
%%
%%    Filelist = filelib:fold_files("/../ebin", ".beam", true, fun(File, Acc) -> [File | Acc] end, []),
%%
%%    F = fun(File, Acc) ->
%%        {ok, {Module, NewVsn}} = beam_lib:version(File),
%%        Acc#{Module => NewVsn}
%%        end,
%%    Vsns = lists:foldl(F, #{}, Filelist),
%%
%%    F2 = fun(M, Acc0) ->
%%        List = M:module_info(attributes),
%%        case lists:keyfind(vsn, 1, List) of
%%            {vsn, Vsn} ->
%%                case maps:find(M, Vsns) of
%%                    {ok, NewVsn} when NewVsn =/= Vsn ->
%%                        [M | Acc0];
%%                    _ ->
%%                        Acc0
%%                end;
%%            _ ->
%%                Acc0
%%        end
%%         end,
%%    Modules = lists:foldl(F2, [], erlang:loaded()),
%%    error_logger:info_msg("Hotup ~p", [Modules]),
%%    hotup(Modules, ?false).
%%hotup(Modules) ->
%%    hotup(Modules, ?true).
%%hotup(Modules, IsSync) ->
%%    {ok, Prepared} = code:prepare_loading(Modules),
%%    [code:purge(M) || M <- Modules],
%%    code:finish_loading(Prepared),
%%    % 是否其他节点也加载
%%    if
%%        IsSync ->
%%            Nodes =
%%                case xg_span:center_node() of
%%                    ?undefined ->
%%                        nodes();
%%                    Node ->
%%                        [Node | nodes()]
%%                end,
%%            rpc:multicall(Nodes, ?MODULE, hotup, [Modules, ?false]);
%%        ?true ->
%%            ok
%%    end,
%%    ok.