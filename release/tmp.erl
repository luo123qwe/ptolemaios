%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 写好复制过去escript
%%% @end
%%%-------------------------------------------------------------------
-module(tmp).
-author("dominic").

%% API
-export([main/1]).

-define(APP_NAME, "ptolemaios").
-define(MAIN_DIR, ?APP_NAME ++ "-main").
-define(STOP_MFA, {ptolemaios, async_stop, [500]}).
-define(UPDATE_MFA, {fix_hot, fix, []}).

main(["tar_main" | Opt]) ->
    tar_main("main", make_opt(Opt));
main(["tar_ebin", Tag | Opt]) ->
    tar_ebin(Tag, make_opt(Opt));
main(["backup_tree", Backup]) ->
    backup_tree(Backup);
main(["update_main"]) ->
    stop(),
    file:del_dir_r(?MAIN_DIR),
    extract_tag(?MAIN_DIR),
    start();
main(["restart"]) ->
    stop(),
    start();
main(["stop"]) ->
    stop();
main(["update_ebin"]) ->
    update_ebin();
main(_) ->
    usage().

make_opt([]) ->
    [];
make_opt([K, V | T]) ->
    [{K, V} | make_opt(T)].

tar_main(Tag, Opt) ->
    %% 先打包
    os:cmd("cd .. && rebar3 as " ++ Tag ++ " tar"),
    %% 复制到这里
    TarName = ?APP_NAME ++ "-main.tar.gz",
    file:copy("../_build/" ++ Tag ++ "/rel/" ++ ?APP_NAME ++ "/" ++ TarName, TarName),
    %% 备份
    backup(TarName, Opt).

tar_ebin(Tag, Opt) ->
    %% 先编译
    os:cmd("cd .. && rebar3 as " ++ Tag ++ " compile"),
    %% 打压缩包
    EBinPath = "../_build/" ++ Tag ++ "/lib/" ++ ?APP_NAME ++ "/ebin",
    case filelib:is_dir(EBinPath) of
        true -> erl_tar:create("ebin.tar.gz", [{".", EBinPath}]);
        _ -> throw({no_ebin, EBinPath})
    end,
    backup("ebin.tar.gz", Opt).

backup(File, Opt) ->
    case lists:keyfind("-backup", 1, Opt) of
        false -> ok;
        {_, Path} ->
            case filelib:is_dir(Path) of
                true -> ok;
                _ -> file:make_dir(Path)
            end,
            Root = filename:rootname(File, ".tar.gz"),
            
            file:copy(File, Path ++ "/" ++ Root ++ "_" ++ date_str() ++ ".tar.gz")
    end.

backup_tree(Backup) ->
    TimeList =
        lists:map(fun(FileName) ->
            case FileName of
                ?APP_NAME ++ "-main_" ++ Time ->
                    {Time, FileName};
                "ebin_" ++ Time ->
                    {Time, FileName}
            end
                  end, Backup),
    TimeList1 = lists:keysort(1, TimeList),
    Tree = backup_tree(TimeList1, undefined, [], []),
    PrettyTree =
        lists:map(fun({MainName, EbinNameList}) ->
            [$\n, MainName, $\n, [[$-, $-, $-, $-, EbinName, $\n] || EbinName <- EbinNameList]]
                  end, Tree),
    io:format("~s~n", [PrettyTree]).

backup_tree([], undefined, _, Tree) ->
    Tree;
backup_tree([], MainName, EBinList, Tree) ->
    [{MainName, EBinList} | Tree];
backup_tree([{_, FileName} | T], MainName, EBinList, Tree) ->
    case FileName of
        ?APP_NAME ++ "-main_" ++ _ ->
            case MainName of
                undefined -> backup_tree(T, FileName, [], Tree);
                _ -> backup_tree(T, FileName, [], [{MainName, EBinList} | Tree])
            end;
        "ebin_" ++ _ ->
            backup_tree(T, MainName, [FileName | EBinList], Tree)
    end.

date_str() ->
    {{Y, M, D}, {H, Min, S}} = erlang:localtime(),
    [date_str_add_zero(Int) || Int <- [Y, M, D, H, Min, S]].

date_str_add_zero(Int) when Int > 9 ->
    integer_to_list(Int);
date_str_add_zero(Int) ->
    [$0, integer_to_list(Int)].

init_node() ->
    {ok, VmArgs} = file:script("vm.args.src"),
    {_, NodeStr} = lists:keyfind("-name", 1, VmArgs),
    Node = list_to_atom(NodeStr),
    {_, CookieStr} = lists:keyfind("-setcookie", 1, VmArgs),
    Cookie = list_to_atom(CookieStr),
    % 开启本节点
    net_kernel:start([list_to_atom("release_" ++ integer_to_list(erlang:system_time(second)) ++ "@127.0.0.1")]),
    erlang:set_cookie(Node, Cookie),
    Node.

start() ->
    case os:type() of
        {unix, _} ->
            os:cmd("cd " ++ ?MAIN_DIR ++ " && nohup ./bin/test console &");
        {win32, _} ->
            spawn(fun() -> os:cmd("cd " ++ ?MAIN_DIR ++ " && call ./bin/test console") end)
    end,
    io:format("start console").

stop() ->
    Node = init_node(),
    %% todo 实现关闭系统
    {M, F, A} = ?STOP_MFA,
    case rpc:call(Node, M, F, A) of
        {badrpc, nodedown} -> ok;
        ok -> timer:sleep(500);
        Error -> throw({stop, Error})
    end.

update_ebin() ->
    %% 解压
    ok = erl_tar:extract("ebin.tar.gz", [{cwd, "ebin"}]),
    %% 移动ebin到app的lib
    lists:any(fun(Lib) ->
        case Lib of
            ?APP_NAME ++ "-" ++ _ ->
                LibEBin = ?MAIN_DIR ++ "/lib/" ++ Lib ++ "/ebin",
                file:del_dir_r(LibEBin),
                file:make_dir(LibEBin),
                copy_file("ebin", LibEBin),
                true;
            _ ->
                false
        end
              end, element(2, file:list_dir(?MAIN_DIR ++ "/lib"))),
    %% 删除更新相关的文件
    file:del_dir_r("ebin"),
    file:delete("ebin.tar.gz"),
    Node = init_node(),
    %% 游戏节点更新或者开启
    case net_kernel:connect_node(Node) of
        true ->
            {M, F, A} = ?UPDATE_MFA,
            io:format("~w~n", [rpc:call(Node, M, F, A)]);
        _ ->
            start()
    end.

extract_tag(NewDir) ->
    try
        ok = erl_tar:extract(NewDir ++ ".tar.gz", [{cwd, NewDir}, compressed]),
        eval_src(NewDir)
    catch
        C:E:S ->
            delete_tar(NewDir),
            erlang:raise(C, E, S)
    end.

eval_src(NewDir) ->
    [_, Version] = string:split(NewDir, "-"),
    ReleaseDir = NewDir ++ "/releases/" ++ Version,
    {ok, VmArgs} = file:script("vm.args.src"),
    %% 至少需要name和setcookie
    case not lists:keymember("-name", 1, VmArgs) orelse not lists:keymember("-setcookie", 1, VmArgs) of
        true ->
            throw(no_name_or_cookie);
        _ -> ok
    end,
    VmArgsIOData =
        lists:map(fun(Element) ->
            case Element of
                {Tag, Args} ->
                    [Tag, $ , Args, $\n];
                _ ->
                    Element
            end end, VmArgs),
    file:write_file(ReleaseDir ++ "/vm.args", VmArgsIOData),
    {ok, Config} = file:script(ReleaseDir ++ "/sys.config.src"),
    ConfigIOData = io_lib:format("~p.", [Config]),
    file:write_file(ReleaseDir ++ "/sys.config", ConfigIOData),
    {VmArgs, Config}.

delete_tar(NewDir) ->
    file:del_dir_r(NewDir),
    file:delete(NewDir ++ ".tar.gz").

copy_file(From, To) ->
    copy_file(From, To, element(2, file:list_dir(From))).

copy_file(From, To, CopyList) ->
    lists:foldl(fun(N, {F, T} = Acc) ->
        FromFilePath = F ++ "/" ++ N,
        CopyFilePath = T ++ "/" ++ N,
        case filelib:is_dir(N) of
            true ->
                file:del_dir_r(CopyFilePath),
                file:make_dir(CopyFilePath),
                copy_file(FromFilePath, CopyFilePath),
                Acc;
            _ ->
                file:copy(FromFilePath, CopyFilePath),
                Acc
        end end, {From, To}, CopyList).

usage() ->
    io:format("
" ++ ?APP_NAME ++ " 发布 工具
基本语法 escript release.escript [cmd] [option]
  tar_main: 使用 rebar3 release 打包
    -backup 备份文件夹
    ").