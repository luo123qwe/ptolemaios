%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 腾讯行为树工具
%%%
%%% 部署流程
%%%
%%% https://www.behaviac.com/language/zh/downloads/下载3.6.39
%%%
%%% 设置导出路径, 行为树导出位置 => behaviac, 代码生成位置 => c_src\behaviac
%%%
%%% 按照教程操作, 编辑c_src\behaviac\behaviac_enif.cpp即可
%%% @end
%%%-------------------------------------------------------------------
-module(behaviac_enif).
-author("dominic").

%% API
-export([run/0]).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

%% @private
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      BeamDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(BeamDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    Threads = erlang:system_info(schedulers),
    ok = erlang:load_nif(filename:join(PrivDir, ?MODULE_STRING), Threads).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).


%% @doc 执行行为树
-spec run() -> any().
run() ->
    ?NOT_LOADED.