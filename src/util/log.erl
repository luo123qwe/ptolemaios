%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 日志
%%%
%%% 使用logger+logger_disk_log_h实现循环日志
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(log).
-author("dominic").

%% API
-export([start/0, stop/0, index/0, set_level/1]).

%% @private 初始化循环日志, 自定义格式
start() ->
    {ok, Config} = application:get_env(ptolemaios, log),
    {_, Level} = lists:keyfind(level, 1, Config),
    {_, Path} = lists:keyfind(log_dir, 1, Config),
    ok = logger:add_handler(ptolemaios, logger_disk_log_h, #{config => #{file => Path,
        type => wrap,
        max_no_files => 10,
        max_no_bytes => 30000},
        level => Level,
        filesync_repeat_interval => 5000}),
    logger:update_formatter_config(ptolemaios, #{template => [level, " ", time, " ", pid, " ", mfa, ":", line, "\n", msg, "\n"]}),
    logger:update_formatter_config(default, #{template => [level, " ", time, " ", pid, " ", mfa, ":", line, "\n", msg, "\n"]}),
    set_level(Level).

%% @private
stop() ->
    logger:remove_handler(ptolemaios).

%% @doc 设置日志等级
-spec set_level(logger:level()) -> ok | {error, term()}.
set_level(Level) ->
    logger:set_primary_config(#{level => Level}).

%% @doc 当前日志索引, 使用ptolemaios的配置
-spec index() -> non_neg_integer().
index() ->
    {ok, Config} = application:get_env(ptolemaios, log),
    {_, Path} = lists:keyfind(log_dir, 1, Config),
    index(Path).

%% @doc 当前日志索引, 传入日志文件夹路径, app没启动的时候也能查
-spec index(file:filename_all()) -> non_neg_integer().
index(Path) ->
    {CurFileNo, _CurFileSz, _TotSz, _NoFiles} = disk_log_1:read_index_file(Path),
    CurFileNo.