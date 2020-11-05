-ifndef(ERROR_CODE_HRL).
-define(ERROR_CODE_HRL, true).

-include("util.hrl").
-define(ERROR_CODE1(ErrorCode), {error_code, ErrorCode}).
-define(THROW_ERROR_CODE1(ErrorCode), throw(?ERROR_CODE1(ErrorCode))).
-define(ERROR_CODE_IF2(Expr, ErrorCode), ?DO_IF(Expr, ?THROW_ERROR_CODE1(ErrorCode))).
-define(ERROR_CODE_IF_NOT2(Expr, ErrorCode), ?DO_IF_NOT(Expr, ?THROW_ERROR_CODE1(ErrorCode))).
-define(ERROR_CODE_MATCH3(Expr, Match, ErrorCode), ?DO_MATCH(Expr, Match, ?THROW_ERROR_CODE1(ErrorCode))).
-define(ERROR_CODE_NOT_MATCH3(Expr, Match, ErrorCode), ?DO_NOT_MATCH(Expr, Match, ?THROW_ERROR_CODE1(ErrorCode))).

%% 通用错误码<10000
-define(ERROR_CODE_ERROR, 0).% 未知错误
%% 模块id * 10000 + 唯一id
-define(ERROR_CODE_HAD_LOGIN, 10001).% 已登录
-define(ERROR_CODE_OTHER_HAD_LOGIN, 10002).% 其他人已登录
-define(ERROR_CODE_NOT_LOGIN, 10003).% 未登录
-define(ERROR_CODE_HAD_REGISTER, 10004).% 角色已注册
-define(ERROR_CODE_NO_ROLE, 10005).% 角色不存在


-endif.