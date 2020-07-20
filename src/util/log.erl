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
-export([init/0, index/0]).

%% 初始化循环日志, 自定义格式
init() ->
    ok = logger:add_handler(ptolemaios, logger_disk_log_h, #{config => #{file => "./log/log",
        type => wrap,
        max_no_files => 10,
        max_no_bytes => 30000},
        filesync_repeat_interval => 5000}),
    logger:update_formatter_config(?MODULE, #{template => [level, " ", time, " ", pid, " ", mfa, ":", line, "\n", msg, "\n"]}),
    logger:update_formatter_config(default, #{template => [level, " ", time, " ", pid, " ", mfa, ":", line, "\n", msg, "\n"]}).


%% @doc 当前日志索引, 只能开启节点的时候看, escript写了感觉不好维护
index() ->
    case disk_log:info(ptolemaios) of
        {error, _} ->
            disk_log_down;
        Info ->
            element(2, lists:keyfind(current_file, 1, Info))
    end.