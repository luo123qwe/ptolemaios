%% break循环, 支持的函数才能用
-define(UTIL_FOLD_BREAK, util_fold_break).
-define(UTIL_FOLD_BREAK(Acc), {util_fold_break, Acc}).

%% 常用的宏
%% IF系列
%% ?IF(1 + 1 > 1, a(), b())
%% ?DO_IF(1 + 1 > 1, a())
-define(IF(Expr, True, False),
    case Expr of
        true -> True;
        false -> False
    end).
-define(IF_NOT(Expr, True, False),
    case not Expr of
        true -> True;
        false -> False
    end).
-define(DO_IF(Expr, True),
    case Expr of
        true -> True;
        false -> skip
    end).
-define(DO_IF_NOT(Expr, True),
    case not Expr of
        true -> True;
        false -> skip
    end).
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
-define(DO_MATCH(Expr, Guards, Match),
    case Expr of
        Guards -> Match;
        _ -> skip
    end).
-define(DO_NOT_MATCH(Expr, Guards, NotMatch),
    case Expr of
        Guards -> skip;
        _ -> NotMatch
    end).
%% 返回错误码专用
-define(ERR_CODE(Code), {error_code, Code}).
-define(ERR_IF(Expr, Code), ?DO_IF(Expr, erlang:throw(?ERR_CODE(Code)))).
-define(ERR_IF_NOT(Expr, Code), ?DO_IF_NOT(Expr, erlang:throw(?ERR_CODE(Code)))).
-define(ERR_MATCH(Expr, Guards, Code), ?DO_MATCH(Expr, Guards, erlang:throw(?ERR_CODE(Code)))).
-define(ERR_NOT_MATCH(Expr, Guards, Code), ?DO_NOT_MATCH(Expr, Guards, erlang:throw(?ERR_CODE(Code)))).


%% 本地节点锁
-define(ETS_LOCAL_LOCK, ets_local_lock).
-record(local_lock, {
    key,
    owner,
    lock = 0
}).

%% 日志
-define(LOG_ALERT(Format), logger:alert(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_ALERT(Format, Args), logger:alert(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_CRITICAL(Format), logger:critical(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_CRITICAL(Format, Args), logger:critical(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_ERROR(Format), logger:error(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_ERROR(Format, Args), logger:error(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_WARNING(Format), logger:warning(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_WARNING(Format, Args), logger:warning(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_NOTICE(Format), logger:notice(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_NOTICE(Format, Args), logger:notice(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_INFO(Format), logger:info(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_INFO(Format, Args), logger:info(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_DEBUG(Format), logger:debug(Format, [], #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).
-define(LOG_DEBUG(Format, Args), logger:debug(Format, Args, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}, line => ?LINE})).

%% kv_op, 默认值, 详情看kv_op.erl -type default()
-define(KV_OP_DEF(F), {kv_op_def, F}).
-define(KV_OP_DEF(MOrF, FOrA), {kv_op_def, MOrF, FOrA}).
-define(KV_OP_DEF(M, F, A), {kv_op_def, M, F, A}).
-define(KV_OP_DEF, '_').