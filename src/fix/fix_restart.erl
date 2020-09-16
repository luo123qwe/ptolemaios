%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fix_restart).
-author("dominic").

-include("util.hrl").
-include("fix.hrl").

%% API
-export([
    system_init/0,
    fix/0, fix/1
]).

-callback run() -> term().

system_init() ->
    file:make_dir(?FIX_DETS_PATH),
    {ok, ?DETS_FIX} = dets:open_file(?DETS_FIX, [{file, ?FIX_DETS_PATH ++ "/" ++ atom_to_list(?DETS_FIX)}]),
    case dets:lookup(?DETS_FIX, ?MODULE) of
        [{_, _Index}] -> skip;
        _ -> system_init(1)
    end,
    dets:close(?DETS_FIX),
    ok.

system_init(Index) ->
    Module = ?FIX_RESTART_MODULE(Index),
    case code:load_file(Module) of
        {module, _} ->
            system_init(Index + 1);
        _ ->
            dets:insert(?DETS_FIX, {?MODULE, Index - 1})
    end.

fix() ->
    fix(1).

fix(DefaultIndex) ->
    %% 只是兼容测试, 因为测试的时候我会直接删掉dets文件夹
    file:make_dir(?FIX_DETS_PATH),
    {ok, ?DETS_FIX} = dets:open_file(?DETS_FIX, [{file, ?FIX_DETS_PATH ++ "/" ++ atom_to_list(?DETS_FIX)}]),
    %% 获取执行下标
    case dets:lookup(?DETS_FIX, ?MODULE) of
        [{_, Index0}] -> Index = Index0 + 1;
        _ -> Index = DefaultIndex
    end,
    %% 执行热更代码
    case Index of
        undefined -> ok;
        _ -> dets:insert(?DETS_FIX, {?MODULE, execute(Index)})
    end,
    dets:close(?DETS_FIX).

%% 执行代码并且返回最后执行的下标
execute(Index) ->
    Module = ?FIX_RESTART_MODULE(Index),
    case code:is_loaded(Module) =/= false orelse element(1, code:load_file(Module)) =/= error of
        true ->
            IsFail =
                try
                    ?LOG_NOTICE("~w:run()", [Module]),
                    Module:run(),
                    false
                catch
                    C:E:S ->
                        ?LOG_ERROR("hot fix fail fail when ~w~n~w ~w~n~p", [Module, C, E, S]),
                        true
                end,
            case IsFail of
                true -> Index;
                _ -> execute(Index + 1)
            end;
        false ->
            Index - 1
    end.