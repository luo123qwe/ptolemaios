%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 热更例子
%%% @end
%%%-------------------------------------------------------------------
-module(example_fix_hot).
-author("dominic").

%% API
-export([
    run_reload/0,
    run_fix_data/0
]).

-record(example, {a, b, c}).

%% 重新加载代码
run_reload() ->
    %% reload本app的代码
    fix_hot:reload_release().

%% 修复进程数据, 把#example.a为2的数据改成1
run_fix_data() ->
    %% 挂起进程
    PidList = fix_hot:suspend([example_sup]),
    %% 重载代码
    fix_hot:reload_release(),
    %% 修改数据库
    vt_sql:query("update example set a=1 where a=2"),
    %% 修改ets数据
    Ets = vt_sql:make_ets_name(example),
    ets:foldl(fun(Record, []) ->
        case Record#example.a of
            2 -> ets:insert(Ets, Record#example{a = 1});
            _ -> skip
        end
              end, [], Ets),
    %% 修改进程的数据
    StateFun = fun(S) ->
        %% 判断需要更新的进程
        case S of
            {example_sup} ->
                S;
            {example_child} ->
                vt_sql:fold_cache(fun(Record, []) ->
                    case Record#example.a of
                        2 -> vt_sql:insert(Record#example{a = 1});
                        _ -> skip
                    end
                                  end, [], example),
                S
        end
               end,
    lists:foreach(fun(Pid) ->
        sys:replace_state(Pid, StateFun)
                  end, PidList),
    %% 恢复进程
    fix_hot:resume(PidList).

