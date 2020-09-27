%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% release.escript
%%% @end
%%%-------------------------------------------------------------------
-module(release).
-author("dominic").

%% API
-export([main/1]).

%% ========private_split_str==========
-include_lib("kernel/include/file.hrl").

-define(RELEASE_TAG, "prod").
-define(STOP_MFA, {ptolemaios_app, async_stop, [500]}).
-define(UPDATE_MFA, {fix_hot, fix, []}).
-define(REUSE_DIR, ["log", "fix_dets", "vt_sql_dets"]).

main(Args) ->
    case make_escript() of
        true ->
            %% 新代码再执行一次
            escript:start();
        _ ->
            do_main(Args)
    end.

%% 打包机用
do_main(["tar" | Opt]) ->
    tar(make_opt(Opt));
do_main(["tar_backup" | Opt]) ->
    tar_backup(make_opt(Opt));
do_main(["tar_ebin" | Opt]) ->
    tar_ebin(make_opt(Opt));
do_main(["backup_tree" | Opt]) ->
    backup_tree(make_opt(Opt));
%% 发布机用
do_main(["replace_tar" | Opt]) ->
    replace_tar(make_opt(Opt));
do_main(["restart" | Opt]) ->
    stop(make_opt(Opt)),
    start();
do_main(["stop" | Opt]) ->
    stop(make_opt(Opt));
do_main(["update_ebin" | Opt]) ->
    update_ebin(make_opt(Opt));
do_main(["update_backup" | Opt]) ->
    update_backup(make_opt(Opt));
do_main(_) ->
    usage().

make_opt([]) ->
    [];
make_opt(["-reuse_dir" | T]) ->
    {T1, SaveDirList} = make_opt_multiple_args(T, []),
    [{"-reuse_dir", SaveDirList} | make_opt(T1)];
make_opt(["-replace" | T]) ->
    {T1, SaveDirList} = make_opt_multiple_args(T, []),
    [{"-replace", SaveDirList} | make_opt(T1)];
make_opt(["-delete" | T]) ->
    {T1, SaveDirList} = make_opt_multiple_args(T, []),
    [{"-delete", SaveDirList} | make_opt(T1)];
make_opt(["-replace_if_exist" | T]) ->
    {T1, SaveDirList} = make_opt_multiple_args(T, []),
    [{"-replace_if_exist", SaveDirList} | make_opt(T1)];
make_opt(["-replace_if_not_exist" | T]) ->
    {T1, SaveDirList} = make_opt_multiple_args(T, []),
    [{"-replace_if_not_exist", SaveDirList} | make_opt(T1)];
make_opt([K, V | T]) ->
    [{K, V} | make_opt(T)].

make_opt_multiple_args([], SaveDirList) ->
    {[], SaveDirList};
make_opt_multiple_args([H | T] = L, SaveDirList) ->
    case H of
        "-" ++ _ ->
            {L, SaveDirList};
        _ ->
            make_opt_multiple_args(T, [H | SaveDirList])
    end.

%% 打包一个版本
tar(Opt) ->
    Tag = proplists:get_value("-profile", Opt, ?RELEASE_TAG),
    {AppName, Version} = get_name_version(Tag),
    io:format("tar ~s-~s~n", [AppName, Version]),
    %% 先打包
    Cmd = "cd .. && \"./rebar3\" as " ++ Tag ++ " tar",
    os:cmd(Cmd),
    TarName = AppName ++ "-" ++ Version ++ ".tar.gz",
    BuildPath = "../_build/" ++ Tag ++ "/rel/" ++ AppName ++ "/" ++ TarName,
    case filelib:is_file(BuildPath) of
        true -> skip;
        _ -> io:format("tar fail, cmd:~n~s~n", [Cmd])
    end,
    %% 复制到这里
    file:copy(BuildPath, "tmp.tar.gz"),
    %% 再套一层打包tar
    ReleaseName = AppName ++ ".tar.gz",
    erl_tar:create(ReleaseName, [{TarName, "tmp.tar.gz"}, "release.escript"]),
    file:delete("tmp.tar.gz"),
    io:format("create tar ~s~n", [ReleaseName]),
    %% 备份
    backup(ReleaseName, Opt).

tar_backup(Opt) ->
    {_, Backup} = lists:keyfind("-backup", 1, Opt),
    {_, Tar} = lists:keyfind("-tar", 1, Opt),
    [AppName, _Date] = string:split(Tar, "_"),
    Tree = make_backup_tree(Backup),
    {_, EBinTarList} = lists:keyfind(Tar, 1, Tree),
    ReleaseName = AppName ++ ".tar.gz",
    erl_tar:create(ReleaseName, [{EBinTar, Backup ++ "/" ++ EBinTar} || EBinTar <- EBinTarList] ++ [{Tar, Backup ++ "/" ++ Tar}, "release.escript"]),
    io:format("tar backup:~n~s~n", [[Tar, $\n, [[EBinTar, $\n] || EBinTar <- EBinTarList]]]).

tar_ebin(Opt) ->
    Tag = proplists:get_value("-profile", Opt, ?RELEASE_TAG),
    {AppName, Version} = get_name_version(Tag),
    io:format("tar ~s-~s~n", [AppName, Version]),
    %% 先编译
    Cmd = "cd .. && \"./rebar3\" as " ++ Tag ++ " compile",
    os:cmd(Cmd),
    EBinPath = "../_build/" ++ Tag ++ "/lib/" ++ AppName ++ "/ebin",
    %% 打压缩包
    case filelib:is_dir(EBinPath) of
        true ->
            erl_tar:create("ebin.tar.gz", [{".", EBinPath}]),
            io:format("create tar ebin.tar.gz~n", []),
            backup("ebin.tar.gz", Opt);
        _ ->
            io:format("tar fail, cmd:~n~s~n", [Cmd])
    end.

get_name_version(_Tag) ->
    {ok, RebarConfig} = file:script("../rebar.config.script"),
    {_, Relx} = lists:keyfind(relx, 1, RebarConfig),
    {_, {Name, Version}, _} = lists:keyfind(release, 1, Relx),
    {to_str(Name), to_str(Version)}.

to_str(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_str(List) when is_list(List) ->
    List.

backup(File, Opt) ->
    case proplists:get_value("-backup", Opt, undefined) of
        undefined ->
            skip;
        Path ->
            case filelib:is_dir(Path) of
                true -> ok;
                _ -> file:make_dir(Path)
            end,
            Root = filename:rootname(File, ".tar.gz"),
            Backup = Path ++ "/" ++ Root ++ "_" ++ date_str() ++ ".tar.gz",
            file:copy(File, Path ++ "/" ++ Root ++ "_" ++ date_str() ++ ".tar.gz"),
            io:format("backup ~s to ~s~n", [File, Backup])
    end.

backup_tree(Opt) ->
    case proplists:get_value("-backup", Opt, undefined) of
        undefined ->
            io:format("no -backup args~n");
        Backup ->
            Tree = make_backup_tree(Backup),
            PrettyTree =
                lists:map(fun({MainName, EbinNameList}) ->
                    [$\n, MainName, $\n, [[$ , $ , $-, $-, EbinName, $\n] || EbinName <- EbinNameList]]
                          end, Tree),
            io:format("~s~n", [PrettyTree])
    end.

make_backup_tree(Backup) ->
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
    make_backup_tree(TimeList1, undefined, [], []).


make_backup_tree([], undefined, _, Tree) ->
    Tree;
make_backup_tree([], MainName, EBinList, Tree) ->
    [{MainName, EBinList} | Tree];
make_backup_tree([{_, FileName} | T], MainName, EBinList, Tree) ->
    case FileName of
        "ebin_" ++ _ ->
            make_backup_tree(T, MainName, [FileName | EBinList], Tree);
        _ ->
            case MainName of
                undefined -> make_backup_tree(T, FileName, [], Tree);
                _ -> make_backup_tree(T, FileName, [], [{MainName, EBinList} | Tree])
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
    Dir = get_dir(),
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
    case node() of
        'nonode@nohost' ->
            Cookie = list_to_atom(CookieStr),
            % 开启本节点
            net_kernel:start([list_to_atom("release_" ++ integer_to_list(erlang:system_time(second)) ++ "@127.0.0.1")]),
            erlang:set_cookie(Node, Cookie),
            Node;
        _Node ->
            Node
    end.

start() ->
    Dir = get_dir(),
    [Name, _Version] = string:split(Dir, "-"),
    case os:type() of
        {unix, _} ->
            os:cmd("cd " ++ Dir ++ " && nohup ./bin/" ++ Name ++ " console &");
        {win32, _} ->
            spawn(fun() -> os:cmd("cd " ++ Dir ++ " && call ./bin/" ++ Name ++ " console") end)
    end,
    Node = init_node(),
    case lists:any(fun(_) ->
        timer:sleep(500),
        case net_kernel:connect_node(Node) of
            true -> true;
            _ -> false
        end end, lists:seq(1, 10)) of
        true ->
            io:format("start console~n");
        _ ->
            throw(start_fail)
    end.

stop(Opt) ->
    {M, F, A} = get_mfa(Opt, ?STOP_MFA),
    Node = init_node(),
    case rpc:call(Node, M, F, A) of
        {badrpc, nodedown} -> ok;
        ok -> timer:sleep(500);
        Error -> throw({stop, Error})
    end.

replace_tar(Opt) ->
    {ok, ListDir} = file:list_dir("."),
    %% 一, escript+N个tar
    %% 二, escript+Dir+1一个tar
    {Dir, Tar} =
        lists:foldl(fun(FN, {D, T} = Acc) ->
            case filelib:is_dir(FN) of
                true ->
                    {FN, T};
                _ ->
                    case filename:extension(FN) of
                        ".gz" -> {D, FN};
                        _ -> Acc
                    end
            end end, {undefined, undefined}, ListDir),
    case Dir of
        undefined ->
            io:format("install tar~n"),
            init_tar(Opt);
        _ ->
            case filelib:is_file("replace_escript_mask") of
                false ->
                    io:format("replace escript~n"),
                    ok = erl_tar:extract(Tar, [{cwd, "."}, {files, ["release.escript"]}, compressed]),
                    file:write_file("replace_escript_mask", []),
                    escript:start();
                _ ->
                    file:delete("replace_escript_mask"),
                    io:format("replace tar~n"),
                    stop(Opt),
                    %% 复制要保存的文件夹
                    case filelib:is_dir("replace_tar_tmp") of
                        true -> file:del_dir_r("replace_tar_tmp");
                        _ -> skip
                    end,
                    file:make_dir("replace_tar_tmp"),
                    SaveDirList = proplists:get_value("-reuse_dir", Opt, ?REUSE_DIR),
                    lists:foreach(fun(FN) ->
                        SaveDir = Dir ++ "/" ++ FN,
                        case filelib:is_dir(SaveDir) of
                            true ->
                                io:format("backup dir ~s~n", [SaveDir]),
                                copy_dir(SaveDir, "replace_tar_tmp/" ++ FN);
                            _ ->
                                skip
                        end
                                  end, SaveDirList),
                    file:del_dir_r(Dir),
                    {ok, TarListDir} = erl_tar:table(Tar, [compressed]),
                    OverwriteList = TarListDir -- ["release.escript"],
                    ok = erl_tar:extract(Tar, [{cwd, "."}, {files, OverwriteList}, compressed]),
                    file:delete(Tar),
                    io:format("extract:~n~s~n", [string:join(OverwriteList, "~n")]),
                    init_tar(Opt)
            end
    end.

init_tar(Opt) ->
    %% 找到app tar 和 ebin tar
    {AppTar, EBinTarList} =
        lists:foldl(fun(FN, {A, EL} = Acc) ->
            case filename:extension(FN) of
                ".gz" ->
                    case FN of
                        "ebin" ++ _ -> {A, [FN | EL]};
                        _ -> {FN, EL}
                    end;
                _ -> Acc
            end end, {undefined, []}, element(2, file:list_dir("."))),
    %% 解压启动app
    case lists:member($-, AppTar) of
        true ->% {app_name}-{version}
            io:format("init tar: ~s~n", [AppTar]),
            App = filename:rootname(AppTar, ".tar.gz"),
            ok = erl_tar:extract(AppTar, [{cwd, App}, compressed]);
        _ ->% {app_name}_{date}
            io:format("init tar_backup: ~s~n", [AppTar]),
            [AppTar1] = element(2, erl_tar:table(AppTar)) -- ["release.escript"],
            ok = erl_tar:extract(AppTar, [{cwd, "."}, {files, [AppTar1]}, compressed]),
            App = filename:rootname(AppTar1, ".tar.gz"),
            ok = erl_tar:extract(AppTar1, [{cwd, App}, compressed]),
            file:delete(AppTar1)
    end,
    %% 复制保存的文件夹
    case filelib:is_dir("replace_tar_tmp") of
        true ->
            SaveDirList = proplists:get_value("-reuse_dir", Opt, ?REUSE_DIR),
            lists:foreach(fun(FN) ->
                case filelib:is_dir("replace_tar_tmp/" ++ FN) of
                    true ->
                        io:format("recover dir ~s~n", [App ++ "/" ++ FN]),
                        copy_dir("replace_tar_tmp/" ++ FN, App ++ "/" ++ FN);
                    _ -> skip
                end
                          end, SaveDirList),
            file:del_dir_r("replace_tar_tmp");
        _ ->
            skip
    end,
    start(),
    Wait = proplists:get_value("-wait", Opt, 1000),
    io:format("wait: ~w~n", [Wait]),
    timer:sleep(Wait),
    %% 更新 ebin tar
    EBinTarList1 = lists:sort(EBinTarList),
    lists:foreach(fun(FN) ->
        update_ebin(FN, Opt)
                  end, EBinTarList1),
    %% 清理文件
    lists:foreach(fun(FN) ->
        file:delete(FN)
                  end, [AppTar | EBinTarList]).

update_ebin(Opt) ->
    update_ebin("ebin.tar.gz", Opt).

update_ebin(Tar, Opt) ->
    {M, F, A} = get_mfa(Opt, ?UPDATE_MFA),
    %% 找到app name
    Dir = get_dir(),
    [Name, _] = string:split(Dir, "-"),
    %% 解压
    ok = erl_tar:extract(Tar, [{cwd, "ebin"}, compressed]),
    io:format("update ebin: ~s~n", [Tar]),
    %% 移动ebin到app的lib
    lists:any(fun(Lib) ->
        case Lib -- Name of
            "-" ++ _Version ->
                LibEBin = Dir ++ "/lib/" ++ Lib ++ "/ebin",
                file:del_dir_r(LibEBin),
                file:make_dir(LibEBin),
                copy_dir("ebin", LibEBin),
                true;
            _ ->
                false
        end
              end, element(2, file:list_dir(Dir ++ "/lib"))),
    %% 删除更新相关的文件
    file:del_dir_r("ebin"),
    file:delete(Tar),
    Node = init_node(),
    %% 游戏节点更新或者开启
    case net_kernel:connect_node(Node) of
        true ->
            io:format("~w~n", [rpc:call(Node, M, F, A)]);
        _ ->
            io:format("node not start~n")
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

update_backup(Opt) ->
    {_, BackupDir} = lists:keyfind("-backup", 1, Opt),
    Tree = make_backup_tree(BackupDir),
    Tag = proplists:get_value("-profile", Opt, ?RELEASE_TAG),
    {Name, _Version} = get_name_version(Tag),
    %% 从小到大排序tar
    TarList =
        lists:foldl(fun({Main, Ebin}, Acc) ->
            [Main | lists:reverse(Ebin)] ++ Acc
                    end, [], Tree),
    TarList1 =
        case lists:keyfind("-start", 1, Opt) of
            {_, StartTar} ->
                update_beam_split_tar_list(TarList, StartTar);
            _ ->
                TarList
        end,
    %% 解压所有tar, 收集路径信息
    PathList =
        lists:map(fun(FN) ->
            FNPath = BackupDir ++ "/" ++ filename:rootname(filename:rootname(FN)),
            ok = erl_tar:extract(BackupDir ++ "/" ++ FN, [{cwd, FNPath}, compressed]),
            case FN of
                "ebin" ++ _ ->
                    {true, FNPath};
                _ ->
                    [AppTar] = element(2, file:list_dir(FNPath))--["release.escript"],
                    App = filename:rootname(filename:rootname(AppTar)),
                    FNPath1 = FNPath ++ "/" ++ App,
                    ok = erl_tar:extract(FNPath ++ "/" ++ AppTar, [{cwd, FNPath1}, compressed]),
                    %% 找到app的lib
                    AppLib =
                        lists:foldl(fun(Lib, Acc) ->
                            case Lib -- Name of
                                "-" ++ _Version ->
                                    Lib;
                                _ -> Acc
                            end
                                    end, undefined, element(2, file:list_dir(FNPath1 ++ "/lib"))),
                    {false, FNPath1 ++ "/lib/" ++ AppLib}
            end end, TarList1),
    
    Tag = proplists:get_value("-profile", Opt, ?RELEASE_TAG),
    %% 更新tar
    try
        ReplaceList = update_beam_std_beam_list(proplists:get_value("-replace", Opt, []), Tag, Name),
        lists:foreach(fun({BeamPath, Beam, ErlPath, ErlSrcPath}) ->
            lists:foreach(fun({IsEbin, Path}) ->
                case IsEbin of
                    false ->
                        file:copy(BeamPath, Path ++ "/ebin/" ++ Beam),
                        file:copy(ErlPath, Path ++ ErlSrcPath);
                    _ ->
                        file:copy(BeamPath, Path ++ "/" ++ Beam)
                end
                          end, PathList)
                      end, ReplaceList),
        DeleteList = update_beam_std_beam_list(proplists:get_value("-delete", Opt, []), Tag, Name),
        lists:foreach(fun({_BeamPath, Beam, _ErlPath, ErlSrcPath}) ->
            lists:foreach(fun({IsEbin, Path}) ->
                case IsEbin of
                    false ->
                        file:delete(Path ++ "/ebin/" ++ Beam),
                        file:delete(Path ++ ErlSrcPath);
                    _ ->
                        file:delete(Path ++ "/" ++ Beam)
                end
                          end, PathList)
                      end, DeleteList),
        ReplaceIfExistList = update_beam_std_beam_list(proplists:get_value("-replace_if_exist", Opt, []), Tag, Name),
        lists:foreach(fun({BeamPath, Beam, ErlPath, ErlSrcPath}) ->
            lists:foreach(fun({IsEbin, Path}) ->
                case filelib:is_file(Path ++ "/ebin/" ++ Beam) of
                    true ->
                        case IsEbin of
                            false ->
                                file:copy(BeamPath, Path ++ "/ebin/" ++ Beam),
                                file:copy(ErlPath, Path ++ ErlSrcPath);
                            _ ->
                                file:copy(BeamPath, Path ++ "/" ++ Beam)
                        end;
                    _ ->
                        skip
                end
                          end, PathList)
                      end, ReplaceIfExistList),
        ReplaceIfNotExistList = update_beam_std_beam_list(proplists:get_value("-replace_if_not_exist", Opt, []), Tag, Name),
        lists:foreach(fun({BeamPath, Beam, ErlPath, ErlSrcPath}) ->
            lists:foreach(fun({IsEbin, Path}) ->
                case filelib:is_file(Path ++ "/ebin/" ++ Beam) of
                    false ->
                        case IsEbin of
                            false ->
                                file:copy(BeamPath, Path ++ "/ebin/" ++ Beam),
                                file:copy(ErlPath, Path ++ ErlSrcPath);
                            _ ->
                                file:copy(BeamPath, Path ++ "/" ++ Beam)
                        end;
                    _ ->
                        skip
                end
                          end, PathList)
                      end, ReplaceIfNotExistList),
        %% 重新打包
        lists:foreach(fun({IsEbin, PNPath}) ->
            case IsEbin of
                true ->
                    erl_tar:create(PNPath ++ ".tar.gz", [{".", PNPath}]),
                    file:del_dir_r(PNPath);
                _ ->
                    %% app-version/lib/app-version/
                    PNPath1 = filename:dirname(filename:dirname(PNPath)),
                    erl_tar:create(PNPath1 ++ ".tar.gz", [{".", PNPath1}]),
                    file:del_dir_r(PNPath1),
                    PNPath2 = filename:dirname(PNPath1),
                    erl_tar:create(PNPath2 ++ ".tar.gz", [{".", PNPath2}]),
                    file:del_dir_r(PNPath2)
            end
                      end, PathList)
    catch
        C:E:S ->
            io:format("update fail ~w:~w~n~p", [C, E, S]),
            %% 删除所有解压的文件夹
            filelib:fold_files(BackupDir, "", false, fun(FN, _) ->
                case filelib:is_dir(FN) of
                    true -> file:del_dir_r(FN);
                    _ -> skip
                end
                                                     end, [])
    end.

update_beam_split_tar_list([H | T], H) ->
    [H | T];
update_beam_split_tar_list([_ | T], StartTar) ->
    update_beam_split_tar_list(T, StartTar).

update_beam_std_beam_list([], _Tag, _AppName) ->
    [];
update_beam_std_beam_list([Module | T], Tag, AppName) ->
    Beam = Module ++ ".beam",
    BeamPath = "../_build/" ++ Tag ++ "/lib/" ++ AppName ++ "/ebin/" ++ Beam,
    case filelib:is_file(BeamPath) of
        false -> io:format("no beam file ~s in ~s~n", [Module, BeamPath]);
        _ -> skip
    end,
    Erl = Module ++ ".erl",
    BuildSrcPath = "../_build/" ++ Tag ++ "/lib/" ++ AppName ++ "/src",
    ErlPath = search_file(BuildSrcPath, Erl),
    case ErlPath of
        [] -> io:format("no src file ~s in ~s~n", [Module, BuildSrcPath]);
        _ -> skip
    end,
    [{BeamPath, Beam, ErlPath, ((((ErlPath -- "../_build/") -- Tag) -- "/lib/") -- AppName)} | update_beam_std_beam_list(T, Tag, AppName)].

search_file(Dir, Name) ->
    lists:foldl(fun(FN, Acc) ->
        case Acc of
            [] ->
                FN1 = Dir ++ "/" ++ FN,
                case FN of
                    Name ->
                        FN1;
                    _ ->
                        case filelib:is_dir(FN1) of
                            true ->
                                search_file(FN1, Name);
                            false ->
                                Acc
                        end
                end;
            _ ->
                Acc
        end
                end, [], element(2, file:list_dir(Dir))).

copy_dir(From, To) ->
    copy_file(From, To, element(2, file:list_dir(From))).

copy_file(From, To, CopyList) ->
    case filelib:is_dir(To) of
        true -> skip;
        _ -> file:make_dir(To)
    end,
    lists:foldl(fun(N, {F, T} = Acc) ->
        FromFilePath = F ++ "/" ++ N,
        CopyFilePath = T ++ "/" ++ N,
        case filelib:is_dir(N) of
            true ->
                file:del_dir_r(CopyFilePath),
                file:make_dir(CopyFilePath),
                copy_dir(FromFilePath, CopyFilePath),
                Acc;
            _ ->
                file:copy(FromFilePath, CopyFilePath),
                Acc
        end end, {From, To}, CopyList).

get_dir() ->
    {ok, FileList} = file:list_dir("."),
    lists:foldl(fun(FN, Acc) ->
        case filelib:is_dir(FN) of
            true -> FN;
            _ -> Acc
        end end, undefined, FileList).

make_escript() ->
    {ok, #file_info{mtime = ScriptMTime}} = file:read_file_info("release.escript"),
    IsMake =
        case file:read_file_info("release.erl") of
            {ok, #file_info{mtime = ErlMTime}} when ErlMTime > ScriptMTime -> true;
            _ -> false
        end,
    case IsMake of
        true ->
            {ok, Bin} = file:read_file("release.erl"),
            [_, EscriptBody] = re:split(Bin, <<"%% ========private_split_str==========">>, [{parts, 2}]),
            file:write_file("release.escript", ["#!/usr/bin/env escript", EscriptBody]),
            io:format("make ~s~n", ["release.escript"]);
        _ ->
            skip
    end,
    IsMake.

%% 命令行支持中文好难弄, 写着凑合
%% tar: 打包一个版本, 根据rebar3.config.script获取app名字和版本
%% tar_backup:  打包一个backup树的分支
%% tar_ebin:    打包ebin, 用于更新
%% backup_tree: 显示backup树
%% replace_tar: 部署或更新一个tar
%% update_ebin: 更新上面打出来的ebin包, 仅替换文件
%% update_backup: 更新backup中的tar, 仅替换文件
usage() ->
    io:format("
release tool
use:    escript release.escript [cmd] [-option value]
notice: release name must equip app name, unless i can't find out with app to update ebin!!!

cmd:
tar:    create a {app_name}.tar.gz
        read app name and version from rebar3.config.script => {release, {Name, Version}, _}
    -profile [tag], tag define in rebar3.config.script => profiles, default " ++ ?RELEASE_TAG ++ "
    -backup [dir], backup tar to dir like {app_name}_yyyymmddhhmmss.tar.gz
    
tar_backup:   create a {app_name}.tar.gz
        read app and ebin from backup dir and pack a 'date version' like backup tree show
    -backup [dir], backup tar to dir like {app_name}_yyyymmddhhmmss.tar.gz
    -tar [app_tar], {app_name}_yyyymmddhhmmss.tar.gz
    
tar_ebin:   tar ebin for update release
            read app name and version from rebar3.config.script => {release, {Name, Version}, _}
    -profile [tag], tag define in rebar3.config.script => profiles, default " ++ ?RELEASE_TAG ++ "
    -backup [dir], backup tar to dir like ebin_yyyymmddhhmmss.tar.gz
    
backup_tree:    show 'app tar' and 'ebin tar' time relation
    -backup [dir], backup dir
    
replace_tar:    update 'app tar', stop -> update/install -> start
                install if no dir(meaning exist an app) in the path
                orelse, update
    -mfa [{M,F,A}], if need stop, default " ++ io_lib:format("~w", [?STOP_MFA]) ++ "
    -wait [time], start -> wait X ms -> update ebin
    -reuse_dir [dir1[ dir2]], reuse these dir when replace, default " ++ string:join(?REUSE_DIR, " ") ++ "
    
restart:    restart, stop -> start

stop:   stop, rpc call mfa to stop the node
    -mfa [{M,F,A}], default " ++ io_lib:format("~w", [?STOP_MFA]) ++ "
    
update_ebin:    update 'app's ebin' only and rpc call mfa to stop the node or start node
                this cmd just replace file, if it effect boot, rel, .etc, you have to tar it again
    -mfa [{M,F,A}], default " ++ io_lib:format("~w", [?STOP_MFA]) ++ "
    
update_backup:    update all tar erl and beam file in backup dir, mainly design for fix
                  module's src path must be the same in every {app_name}_yyyymmddhhmmss.tar.gz
                  this cmd just replace file, if it effect boot, rel, .etc, you have to tar it again
                  replace -> delete -> replace_if_exist
    -backup [dir], backup dir
    -start [tar], which tar start execute, default it's oldest tar
    -profile [tag], tag define in rebar3.config.script => profiles, default " ++ ?RELEASE_TAG ++ "
    -replace [module1[ module2]], replace module list
    -delete [module1[ module2]], delete module list
    -replace_if_exist [module1[ module2]], repalce module list if module exist
    -replace_if_not_exist [module1[ module2]], repalce module list if module not exist
").