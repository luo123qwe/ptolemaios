%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 热更模板
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(template_fix_hot).
-author("dominic").

-behaviour(plm_fix).

-include("plm_lib.hrl").

%% API
-export([
    fix/0,
    fix_again/0
]).

-record(template_fix_hot, {a, b, c}).

-define(MSG_TEMPLATE_FIX_HOT1(A), {template_fix_hot, A}).

%% 修复
fix() ->
    FixTable = template_fix_hot,
    SuspendList = [template_fix_sup],
    %% 挂起进程
    PidList = plm_fix_hot:suspend(SuspendList),
    try
        %% 重载代码
        plm_fix_hot:reload_release([ptolemaios]),
        %% 修复dets数据
        DetsFixFun = fun({delete, ThisTable, Key}) when ThisTable == FixTable ->
            plm_sql:delete(FixTable, Key);
            (#template_fix_hot{} = Record) ->
                plm_sql:insert(Record);
            (NotExcept) ->
                exit({not_except, NotExcept})
                     end,
        plm_sql:fix_dets(undefined, DetsFixFun, undefined),
        %% 修复数据库
        plm_sql:query("update template_fix_hot set a=1 where a=2"),
        %% 复制 game_db_define:get 的即可
        NewPlmCfg = #plm_sql_cfg{
            db = template,
            record = #template_fix_hot{
                a = #plm_sql_field{type = ?PLM_DB_UINT32, key_type = ?PLM_SQL_KEY_TYPE_PRIVATE},
                b = #plm_sql_field{type = ?PLM_DB_UINT32},
                c = #plm_sql_field{type = ?PLM_DB_UINT32}
            },
            record_fields = record_info(fields, template_fix_hot)
        },
        %% 修复全局数据
        plm_sql:hotfix_update(fun(#template_fix_hot{} = Record) ->
            case Record#template_fix_hot.a of
                1 -> {insert, Record#template_fix_hot{a = 2}};
                _ -> skip
            end
                              end, NewPlmCfg),
        %% 修复进程的数据
        StateFun = fun(S) ->
            plm_sql:hotfix_process_update(NewPlmCfg, fun(#template_fix_hot{} = Record) ->
                case Record#template_fix_hot.a of
                    1 -> plm_sql:insert(Record#template_fix_hot{a = 2});
                    _ -> skip
                end
                                                     end),
            %% 修复消息列表
            Msg = ?MSG_TEMPLATE_FIX_HOT1(1),
            CastMsg = plm_svr:cast_warp(Msg),
            receive
                Msg -> plm_svr:send_imm(self(), ?MSG_TEMPLATE_FIX_HOT1(2));
                CastMsg -> plm_svr:cast_imm(self(), plm_svr:cast_warp(?MSG_TEMPLATE_FIX_HOT1(2)))
            after 0 ->
                ok
            end,
            %% 这里也可以按需修复State里的数据
            S
                   end,
        lists:foreach(fun(Pid) ->
            sys:replace_state(Pid, StateFun)
                      end, PidList),
        %% 修复离线数据, 例如玩家数据
        %% 一般来说数据都是按需加载到内存
        %% 如果sql不好处理就需要加载到内存再处理
        lists:foreach(fun(Id) ->
            case plm_sql:lookup(template_fix_hot, [Id]) of
                #template_fix_hot{a = 1} -> plm_sql:delete(template_fix_hot, [Id]);
                _ -> skip
            end
                      end, []),
        %% 一次性全部同步到数据
        plm_sql:sync_to_db()
    catch
        C:E:S ->
            erlang:raise(C, E, S)
    after
        %% 恢复进程
        plm_fix_hot:resume(PidList)
    end.

%% 第一次修复失败时(可能调用了fix(), 也可能调用了用fix_again()), 再次进行修复调用这个
fix_again() ->
    %% 假设上面StateFun报错了, 这里就不需要再reload和修复ets以及数据库
    %% 如果是幂等的直接再调一次fix()即可
    FixTable = template_fix_hot,
    SuspendList = [template_fix_sup],
    %% 挂起进程
    PidList = plm_fix_hot:suspend(SuspendList),
    try
        %% 这里代码和上面一样了, 就不重复写了
        ok
    catch
        C:E:S ->
            erlang:raise(C, E, S)
    after
        %% 恢复进程
        plm_fix_hot:resume(PidList)
    end.