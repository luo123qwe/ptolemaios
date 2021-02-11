-ifndef(GAME_HRL).
-define(GAME_HRL, true).

-include("plm_lib.hrl").
%% ec = error code
-define(M11_EC1(ErrorCode), {ec, ErrorCode}).
-define(M11_THROW_EC1(ErrorCode), throw(?M11_EC1(ErrorCode))).
-define(M11_IF2(Expr, ErrorCode), ?DO_IF(Expr, ?M11_THROW_EC1(ErrorCode))).
-define(M11_IF_NOT2(Expr, ErrorCode), ?DO_IF_NOT(Expr, ?M11_THROW_EC1(ErrorCode))).
-define(M11_MATCH3(Expr, Match, ErrorCode), ?DO_MATCH(Expr, Match, ?M11_THROW_EC1(ErrorCode))).
-define(M11_NOT_MATCH3(Expr, Match, ErrorCode), ?DO_NOT_MATCH(Expr, Match, ?M11_THROW_EC1(ErrorCode))).

%% 通用错误码<10000
-define(M11_EC_ERROR, 0).% 未知错误
%% 模块id * 10000 + 唯一id
-define(M11_EC_HAD_LOGIN, 120001).% 已登录
-define(M11_EC_OTHER_HAD_LOGIN, 120002).% 其他人已登录
-define(M11_EC_NOT_LOGIN, 120003).% 未登录
-define(M11_EC_HAD_REGISTER, 120004).% 角色已注册
-define(M11_EC_NO_ROLE, 120005).% 角色不存在

-endif.