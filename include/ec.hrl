-ifndef(EC_HRL).
-define(EC_HRL, true).

-include("ptolemaios_lib.hrl").
-define(EC1(ErrorCode), {ec, ErrorCode}).
-define(THROW_EC1(ErrorCode), throw(?EC1(ErrorCode))).
-define(EC_IF2(Expr, ErrorCode), ?DO_IF(Expr, ?THROW_EC1(ErrorCode))).
-define(EC_IF_NOT2(Expr, ErrorCode), ?DO_IF_NOT(Expr, ?THROW_EC1(ErrorCode))).
-define(EC_MATCH3(Expr, Match, ErrorCode), ?DO_MATCH(Expr, Match, ?THROW_EC1(ErrorCode))).
-define(EC_NOT_MATCH3(Expr, Match, ErrorCode), ?DO_NOT_MATCH(Expr, Match, ?THROW_EC1(ErrorCode))).

%% 通用错误码<10000
-define(EC_ERROR, 0).% 未知错误
%% 模块id * 10000 + 唯一id
-define(EC_HAD_LOGIN, 10001).% 已登录
-define(EC_OTHER_HAD_LOGIN, 10002).% 其他人已登录
-define(EC_NOT_LOGIN, 10003).% 未登录
-define(EC_HAD_REGISTER, 10004).% 角色已注册
-define(EC_NO_ROLE, 10005).% 角色不存在


-endif.