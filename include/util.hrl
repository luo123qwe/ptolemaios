-ifndef(UTIL_HRL).
-define(UTIL_HRL, true).

%% break循环, 支持的函数才能用
-define(FOLD_BREAK, fold_break).
-define(FOLD_BREAK1(Acc), {fold_break, Acc}).

%% 常用的宏
%% Match系列
%% Guards里的变量想要使用的话, 需要加上 '= _', idea才不会报错
%% ?MATCH(1 + 1, 2, a(), b())
%% ?DO_MATCH(1 + 1, 2, a())
%% ?MATCH(1 + 1, #a{a = A} = _, a(), b()), AA = A + 1% 此处A不会提示错误
-define(MATCH(Expr, Guards, Match, NotMatch),
    case Expr of
        Guards -> Match;
        _ -> NotMatch
    end).
-define(DO_MATCH(Expr, Guards, Match), ?MATCH(Expr, Guards, Match, skip)).
-define(DO_NOT_MATCH(Expr, Guards, NotMatch), ?MATCH(Expr, Guards, skip, NotMatch)).
%% IF系列
%% ?IF(1 + 1 > 1, a(), b())
%% ?DO_IF(1 + 1 > 1, a())
-define(IF(Expr, True, False), ?MATCH(Expr, true, True, False)).
-define(IF_NOT(Expr, True, False), ?MATCH(Expr, false, True, False)).
-define(DO_IF(Expr, True), ?IF(Expr, True, skip)).
-define(DO_IF_NOT(Expr, True), ?IF_NOT(Expr, True, skip)).


%% 本地节点锁
-define(ETS_LOCAL_LOCK, ets_local_lock).
-record(local_lock, {
    key,
    owner,
    lock = 0
}).

%% 日志
-define(LOG_ALERT, logger:alert([], [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_ALERT(Format), logger:alert(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_ALERT(Format, Args), logger:alert(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_CRITICAL, logger:critical([], [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_CRITICAL(Format), logger:critical(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_CRITICAL(Format, Args), logger:critical(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_ERROR, logger:error([], [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_ERROR(Format), logger:error(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_ERROR(Format, Args), logger:error(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_WARNING, logger:warning([], [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_WARNING(Format), logger:warning(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_WARNING(Format, Args), logger:warning(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_NOTICE, logger:notice([], [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_NOTICE(Format), logger:notice(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_NOTICE(Format, Args), logger:notice(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_INFO, logger:info([], [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_INFO(Format), logger:info(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_INFO(Format, Args), logger:info(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_DEBUG, logger:debug([], [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_DEBUG(Format), logger:debug(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_DEBUG(Format, Args), logger:debug(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).

%% kv_op, 默认值, 详情看kv_op.erl -type default()
-define(KV_OP_DEF(F), {kv_op_def, F}).
-define(KV_OP_DEF(MOrF, FOrA), {kv_op_def, MOrF, FOrA}).
-define(KV_OP_DEF(M, F, A), {kv_op_def, M, F, A}).
-define(KV_OP_DEF, '_').

-endif.