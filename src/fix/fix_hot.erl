%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 热更新
%%% @end
%%%-------------------------------------------------------------------
-module(fix_hot).
-author("dominic").

-include("util.hrl").

%% API
-export([fix/0]).

fix() ->
    ?LOG_ERROR  .

%% @doc 重新加载变更的代码
%%reload() ->
%%    SearchPathList =
%%        lists:foldl(fun(CodePath, Acc) ->
%%
%%                    end, code:get_path()),
%%
%%    Filelist = filelib:fold_files("/../ebin", ".beam", true, fun(File, Acc) -> [File | Acc] end, []),
%%
%%    F = fun(File, Acc) ->
%%        {ok, {Module, NewVsn}} = beam_lib:version(File),
%%        Acc#{Module => NewVsn}
%%        end,
%%    Vsns = lists:foldl(F, #{}, Filelist),
%%
%%    F2 = fun(M, Acc0) ->
%%        List = M:module_info(attributes),
%%        case lists:keyfind(vsn, 1, List) of
%%            {vsn, Vsn} ->
%%                case maps:find(M, Vsns) of
%%                    {ok, NewVsn} when NewVsn =/= Vsn ->
%%                        [M | Acc0];
%%                    _ ->
%%                        Acc0
%%                end;
%%            _ ->
%%                Acc0
%%        end
%%         end,
%%    Modules = lists:foldl(F2, [], erlang:loaded()),
%%    error_logger:info_msg("Hotup ~p", [Modules]),
%%    hotup(Modules, ?false).
%%hotup(Modules) ->
%%    hotup(Modules, ?true).
%%hotup(Modules, IsSync) ->
%%    {ok, Prepared} = code:prepare_loading(Modules),
%%    [code:purge(M) || M <- Modules],
%%    code:finish_loading(Prepared),
%%    % 是否其他节点也加载
%%    if
%%        IsSync ->
%%            Nodes =
%%                case xg_span:center_node() of
%%                    ?undefined ->
%%                        nodes();
%%                    Node ->
%%                        [Node | nodes()]
%%                end,
%%            rpc:multicall(Nodes, ?MODULE, hotup, [Modules, ?false]);
%%        ?true ->
%%            ok
%%    end,
%%    ok.