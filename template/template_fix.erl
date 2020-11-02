%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 热更模板
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(template_fix).
-author("dominic").

-behaviour(fix).

%% API
-export([
    fix/0,
    fix_again/0
]).

-record(template_fix, {a, b, c}).

%% 修复
fix() ->
    %% 挂起进程
    PidList = fix_hot:suspend([template_fix_sup]),
    try
        %% 重载代码
        fix_hot:reload_release(),
        %% 修改数据库
        vt_sql:query("update template_fix set a=1 where a=2"),
        %% 修改ets数据
        Ets = vt_sql:make_ets_name(template_fix),
        ets:foldl(fun(Record, []) ->
            case Record#template_fix.a of
                2 -> ets:insert(Ets, Record#template_fix{a = 1});
                _ -> skip
            end
                  end, [], Ets),
        %% 修改进程的数据
        StateFun = fun(S) ->
            %% 判断需要更新的进程
            case S of
                {template_fix_sup} ->
                    S;
                {template_fix_child} ->
                    vt_sql:fold_cache(fun(Record, []) ->
                        case Record#template_fix.a of
                            2 -> vt_sql:insert(Record#template_fix{a = 1});
                            _ -> skip
                        end
                                      end, [], template_fix),
                    S
            end
                   end,
        lists:foreach(fun(Pid) ->
            sys:replace_state(Pid, StateFun)
                      end, PidList)
    catch
        C:E:S ->
            erlang:raise(C, E, S)
    after
        %% 恢复进程
        fix_hot:resume(PidList)
    end.

%% 上面修复报错时, 再次进行修复调用这个
fix_again() ->
    %% 假设上面StateFun报错了, 这里就不需要再reload和修复ets以及数据库
    %% 挂起进程
    PidList = fix_hot:suspend([template_fix_sup]),
    try
        %% 修改进程的数据
        StateFun = fun(S) ->
            %% 判断需要更新的进程
            case S of
                {template_fix_sup} ->
                    S;
                {template_fix_child} ->
                    vt_sql:fold_cache(fun(Record, []) ->
                        case Record#template_fix.a of
                            2 -> vt_sql:insert(Record#template_fix{a = 1});
                            _ -> skip
                        end
                                      end, [], template_fix),
                    S;
                _ ->% 增加容错
                    S
            end
                   end,
        lists:foreach(fun(Pid) ->
            sys:replace_state(Pid, StateFun)
                      end, PidList)
    catch
        C:E:S ->
            erlang:raise(C, E, S)
    after
        %% 恢复进程
        fix_hot:resume(PidList)
    end.