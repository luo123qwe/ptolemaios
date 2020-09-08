#!/usr/bin/env escript

-define(RELEASE_TAG, "prod").
-define(BACKUP, "backup").
-define(STOP_MFA, {ptolemaios, async_stop, [500]}).
-define(UPDATE_MFA, {fix_hot, fix, []}).

%% 打包机用
main(["tar" | Opt]) ->
    tar(make_opt(Opt));
main(["tar_ebin" | Opt]) ->
    tar_ebin(make_opt(Opt));
main(["backup_tree" | Opt]) ->
    backup_tree(make_opt(Opt));
%% 发布机用
main(["replace_tar" | Opt]) ->
    replace_tar(make_opt(Opt));
main(["restart" | Opt]) ->
    stop(make_opt(Opt)),
    start();
main(["stop" | Opt]) ->
    stop(make_opt(Opt));
main(["update_ebin" | Opt]) ->
    update_ebin(make_opt(Opt));
main(_) ->
    usage().

make_opt([]) ->
    [];
make_opt([K, V | T]) ->
    [{K, V} | make_opt(T)].

%% 打包一个版本
tar(Opt) ->
    Tag = proplists:get_value("-profile", Opt, ?RELEASE_TAG),
    {AppName, Version} = get_name_version(Tag),
    io:format("tar ~s-~s~n", [AppName, Version]),
    %% 先打包
    os:cmd("cd .. && rebar3 as " ++ Tag ++ " tar"),
    io:format("tar success~n"),
    %% 复制到这里
    TarName = AppName ++ "-" ++ Version ++ ".tar.gz",
    file:copy("../_build/" ++ Tag ++ "/rel/" ++ AppName ++ "/" ++ TarName, "tmp.tar.gz"),
    %% 再套一层打包tar
    ReleaseName = AppName ++ ".tar.gz",
    erl_tar:create(ReleaseName, [{TarName, "tmp.tar.gz"}, "release.escript"]),
    file:delete("tmp.tar.gz"),
    io:format("create tar ~s~n", [ReleaseName]),
    %% 备份
    backup(ReleaseName, Opt).

tar_ebin(Opt) ->
    Tag = proplists:get_value("-profile", Opt, ?RELEASE_TAG),
    {AppName, Version} = get_name_version(Tag),
    io:format("tar ~s-~s~n", [AppName, Version]),
    %% 先编译
    os:cmd("cd .. && rebar3 as " ++ Tag ++ " compile"),
    %% 打压缩包
    EBinPath = "../_build/" ++ Tag ++ "/lib/" ++ AppName ++ "/ebin",
    case filelib:is_dir(EBinPath) of
        true -> erl_tar:create("ebin.tar.gz", [{".", EBinPath}]);
        _ -> throw({no_ebin, EBinPath})
    end,
    io:format("create tar ebin.tar.gz~n", []),
    backup("ebin.tar.gz", Opt).

get_name_version(Tag) ->
    {ok, RebarConfig} = file:script("../rebar.config.script"),
    case Tag of
        "default" ->
            {_, Relx} = lists:keyfind(relx, 1, RebarConfig),
            {_, {Name, Version}, _} = lists:keyfind(release, 1, Relx),
            {to_str(Name), to_str(Version)};
        _ ->
            {_, Profiles} = lists:keyfind(profiles, 1, RebarConfig),
            {_, Config} = lists:keyfind(list_to_atom(Tag), 1, Profiles),
            case lists:keyfind(relx, 1, Config) of
                false -> get_name_version("default");
                {_, Relx} ->
                    case lists:keyfind(release, 1, Relx) of
                        false -> get_name_version("default");
                        {_, {Name, Version}, _} ->
                            {to_str(Name), to_str(Version)}
                    end
            end
    end.

to_str(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_str(List) when is_list(List) ->
    List.

backup(File, Opt) ->
    Path = proplists:get_value("-backup", Opt, ?BACKUP),
    case filelib:is_dir(Path) of
        true -> ok;
        _ -> file:make_dir(Path)
    end,
    Root = filename:rootname(File, ".tar.gz"),
    Backup = Path ++ "/" ++ Root ++ "_" ++ date_str() ++ ".tar.gz",
    file:copy(File, Path ++ "/" ++ Root ++ "_" ++ date_str() ++ ".tar.gz"),
    io:format("backup ~s to ~s~n", [File, Backup]).

backup_tree(Opt) ->
    Backup = proplists:get_value("-backup", Opt, ?BACKUP),
    TimeList =
        lists:map(fun(FileName) ->
            case FileName of
                "ebin_" ++ Time ->
                    {Time, FileName};
                _ ->
                    Time = lists:last(string:split(FileName, "_")),
                    {Time, FileName}
            end
                  end, element(2, file:list_dir(Backup))),
    TimeList1 = lists:keysort(1, TimeList),
    Tree = backup_tree(TimeList1, undefined, [], []),
    PrettyTree =
        lists:map(fun({MainName, EbinNameList}) ->
            [$\n, MainName, $\n, [[$ , $ , $-, $-, EbinName, $\n] || EbinName <- EbinNameList]]
                  end, Tree),
    io:format("~s~n", [PrettyTree]).

backup_tree([], undefined, _, Tree) ->
    Tree;
backup_tree([], MainName, EBinList, Tree) ->
    [{MainName, EBinList} | Tree];
backup_tree([{_, FileName} | T], MainName, EBinList, Tree) ->
    case FileName of
        "ebin_" ++ _ ->
            backup_tree(T, MainName, [FileName | EBinList], Tree);
        _ ->
            case MainName of
                undefined -> backup_tree(T, FileName, [], Tree);
                _ -> backup_tree(T, FileName, [], [{MainName, EBinList} | Tree])
            end
    end.

date_str() ->
    {{Y, M, D}, {H, Min, S}} = erlang:localtime(),
    [date_str_add_zero(Int) || Int <- [Y, M, D, H, Min, S]].

date_str_add_zero(Int) when Int > 9 ->
    integer_to_list(Int);
date_str_add_zero(Int) ->
    [$0, integer_to_list(Int)].

init_node() ->
    [Dir] = element(2, file:list_dir(".")) -- ["release.escript"],
    ReleaseDir = Dir ++ "/releases",
    %% 在vm.args中找到节点信息
    {NodeStr, CookieStr} =
        lists:foldl(fun(FN, Acc) ->
            FP = ReleaseDir ++ "/" ++ FN,
            case filelib:is_dir(FP) of
                true ->
                    {ok, Bin} = file:read_file(FP ++ "/vm.args"),
                    {match, ["-name " ++ NS]} = re:run(Bin, "-name .*", [{capture, first, list}]),
                    {match, ["-setcookie " ++ CS]} = re:run(Bin, "-setcookie .*", [{capture, first, list}]),
                    {NS, CS};
                _ ->
                    Acc
            end
                    end, undefined, element(2, file:list_dir(ReleaseDir))),
    Node = list_to_atom(NodeStr),
    Cookie = list_to_atom(CookieStr),
    % 开启本节点
    net_kernel:start([list_to_atom("release_" ++ integer_to_list(erlang:system_time(second)) ++ "@127.0.0.1")]),
    erlang:set_cookie(Node, Cookie),
    Node.

start() ->
    [Dir] = element(2, file:list_dir(".")) -- ["release.escript"],
    [Name, _Version] = string:split(Dir, "-"),
    case os:type() of
        {unix, _} ->
            os:cmd("cd " ++ Dir ++ " && nohup ./bin/" ++ Name ++ " console &");
        {win32, _} ->
            spawn(fun() -> os:cmd("cd " ++ Dir ++ " && call ./bin/" ++ Name ++ " console") end)
    end,
    io:format("start console").

stop(Opt) ->
    {M, F, A} = get_mfa(Opt, ?STOP_MFA),
    Node = init_node(),
    case rpc:call(Node, M, F, A) of
        {badrpc, nodedown} -> ok;
        ok -> timer:sleep(500);
        Error -> throw({stop, Error})
    end.

replace_tar(_Opt) ->
    {ok, ListDir} = file:list_dir("."),
    {Tar, Dir} =
        lists:foldl(fun(FN, {T, D}) ->
            case filelib:is_dir(FN) of
                false -> {FN, D};
                _ -> {T, FN}
            end end, {undefined, undefined}, ListDir -- ["release.escript"]),
    case length(ListDir) of
        2 -> skip;
        3 -> file:del_dir_r(Dir)
    end,
    ok = erl_tar:extract(Tar, [{cwd, filename:rootname(filename:rootname(Tar))}, compressed]),
    file:delete(Tar),
    start().

update_ebin(Opt) ->
    {M, F, A} = get_mfa(Opt, ?UPDATE_MFA),
    %% 找到app name
    [Dir] = element(2, file:list_dir(".")) -- ["release.escript"],
    Name =
        lists:foldl(fun(FN, Acc) ->
            case filename:extension(FN) of
                ".rel" ->
                    filename:rootname(FN);
                _ ->
                    Acc
            end
                    end, undefined, Dir ++ "/releases"),
    %% 解压
    ok = erl_tar:extract("ebin.tar.gz", [{cwd, "ebin"}]),
    %% 移动ebin到app的lib
    lists:any(fun(Lib) ->
        case Lib -- Name of
            "-" ++ _Version ->
                LibEBin = Dir ++ "/lib/" ++ Lib ++ "/ebin",
                file:del_dir_r(LibEBin),
                file:make_dir(LibEBin),
                copy_file("ebin", LibEBin),
                true;
            _ ->
                false
        end
              end, element(2, file:list_dir(Dir ++ "/lib"))),
    %% 删除更新相关的文件
    file:del_dir_r("ebin"),
    file:delete("ebin.tar.gz"),
    Node = init_node(),
    %% 游戏节点更新或者开启
    case net_kernel:connect_node(Node) of
        true ->
            io:format("~w~n", [rpc:call(Node, M, F, A)]);
        _ ->
            start()
    end.

get_mfa(Opt, Default) ->
    case lists:keyfind("-mfa", 1, Opt) of
        {_, MFAStr} ->
            {ok, Tokens, _} = erl_scan:string(MFAStr ++ [$.]),
            {ok, Exprs} = erl_parse:parse_exprs(Tokens),
            {value, Value, _} = erl_eval:exprs(Exprs, []),
            Value;
        _ ->
            Default
    end.

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
release tool
use:    escript release.escript [cmd] [-option value]
notice: release name must equip app name, unless i can't find out with app to update ebin!!!

cmd:
tar:    create a {app_name}.tar.gz
        read app name and version from rebar3.config.script => {release, {Name, Version}, _}
    -profile [tag], tag define in rebar3.config.script => profiles, default " ++ ?RELEASE_TAG ++ "
    -backup [dir], backup tar to dir like {app_name}-{tag}_yyyymmddhhmmss.tar.gz, default" ++ ?BACKUP ++ "

tar_ebin:   tar ebin for update release
            read app name and version from rebar3.config.script => {release, {Name, Version}, _}
    -profile [tag], tag define in rebar3.config.script => profiles, default " ++ ?RELEASE_TAG ++ "
    -backup [dir], backup tar to dir like ebin_yyyymmddhhmmss.tar.gz, default " ++ ?BACKUP ++ "

backup_tree:    show 'app tar' and 'ebin tar' time relation
    -backup [dir], backup dir, default " ++ ?BACKUP ++ "

replace_tar:    update 'app tar', stop -> update/install -> start
                install if dir only include \"app_name-version.tar.gz\" and \"release.escript\"
                update if dir only include \"app_name-version\" and \"app_name-maybe_diff_version.tar.gz\" and \"release.escript\"

restart:    restart, stop -> start

stop:   stop, rpc call mfa to stop the node
    -mfa [{M, F, A}], default " ++ io_lib:format("~w", [?STOP_MFA]) ++ "

update_ebin:    update 'app's ebin' only and rpc call mfa to stop the node or start node
    -mfa [{M, F, A}], default " ++ io_lib:format("~w", [?STOP_MFA]) ++ "
").