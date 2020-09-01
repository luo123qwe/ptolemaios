%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 日志
%%% @end
%%%-------------------------------------------------------------------
-module(log).
-author("dominic").

%% API
-export([init/0, index/0, set_level/1]).

%% 初始化循环日志, 自定义格式
init() ->
    ok = logger:add_handler(ptolemaios, logger_disk_log_h, #{config => #{file => "./log/log",
        type => wrap,
        max_no_files => 10,
        max_no_bytes => 30000},
        level => info,
        filesync_repeat_interval => 5000}),
    logger:update_formatter_config(?MODULE, #{template => [level, " ", time, " ", pid, " ", mfa, ":", line, "\n", msg, "\n"]}),
    logger:update_formatter_config(default, #{template => [level, " ", time, " ", pid, " ", mfa, ":", line, "\n", msg, "\n"]}).

set_level(Level) ->
    logger:set_primary_config(#{level => Level}).

%% @doc 当前日志索引
index() ->
    {CurFileNo, _CurFileSz, _TotSz, _NoFiles} = disk_log_1:read_index_file("log/log"),
    CurFileNo.