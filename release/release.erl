%% @private
%%%-------------------------------------------------------------------
%%% @author dominic
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 发布脚本
%%%
%%% 按照otp23+项目中rebar3生成的结构写的
%%%
%%% 大部分内容直接硬编码, 不考虑兼容和扩展
%%% @end
%%%-------------------------------------------------------------------
-module(release).
-author("dominic").

%% API
-export([main/1]).

-include_lib("kernel/include/file.hrl").

-define(APP_NAME, "ptolemaios").
-define(APP_VERSION, "0.1.0").
-define(APP_FULL_NAME, ?APP_NAME ++ "-" ++ ?APP_VERSION).
-define(RELEASE_TAG, "prod").
-define(STOP_MFA, {ptolemaios_app, async_stop, [500]}).
-define(UPDATE_MFA, {plm_fix_hot, fix, []}).
-define(REUSE_DIR, ["log", "fix_dets", "plm_sql_dets"]).
-define(PD_RELEASE_DATE_STR, pd_release_date_str).

%% 打包机用
main(["tar" | Opt]) ->
    tar(make_opt(Opt));
main(["tar_backup" | Opt]) ->
    tar_backup(make_opt(Opt));
main(["tar_update" | Opt]) ->
    tar_update(make_opt(Opt));
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
main(["update_app" | Opt]) ->
    update_app(make_opt(Opt));
main(_) ->
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
    Profile = proplists:get_value("-profile", Opt, ?RELEASE_TAG),
    io:format("tar ~s~n", [?APP_FULL_NAME]),
    %% 先打包
    Cmd = "cd .. && \"./rebar3\" as " ++ Profile ++ " tar",
    os:cmd(Cmd),
    TarName = ?APP_FULL_NAME ++ ".tar.gz",
    BuildPath = "../_build/" ++ Profile ++ "/rel/" ++ ?APP_NAME ++ "/" ++ TarName,
    case filelib:is_file(BuildPath) of
        true -> skip;
        _ -> io:format("tar fail, cmd:~n~s~n", [Cmd])
    end,
    %% 复制到这里
    file:copy(BuildPath, "tmp.tar.gz"),
    %% 再套一层打包tar
    ReleaseName = ?APP_NAME ++ ".tar.gz",
    write_rebar3_config(["%% tar at ", date_str(), $\n]),
    erl_tar:create(ReleaseName, [{TarName, "tmp.tar.gz"}, "release.erl", "rebar.config.script"]),
    file:delete("tmp.tar.gz"),
    io:format("create tar ~s~n", [ReleaseName]),
    %% 备份
    backup(ReleaseName, Opt).

tar_backup(Opt) ->
    {_, Backup} = lists:keyfind("-backup", 1, Opt),
    {_, Date} = lists:keyfind("-date", 1, Opt),
    Tree = make_backup_tree(Backup),
    %% 找到打包的tar
    MainTarName = ?APP_NAME ++ "_" ++ Date,
    MainTar = MainTarName ++ ".tar.gz",
    TmpPath = Backup ++ "/" ++ MainTarName,
    case lists:keyfind(MainTar, 1, Tree) of
        false ->
            io:format("no tar ~s~n", [MainTar]);
        {_, []} ->% 没有update
            %% 解包
            erl_tar:extract(Backup ++ "/" ++ MainTar, [{cwd, TmpPath}, compressed]),
            {ok, Bin} = file:read_file(TmpPath ++ "/rebar.config.script"),
            ok = file:write_file("rebar.config.script", ["%% tar backup at ", date_str(), $\n, "%% ", MainTar, $\n, Bin]),
            file:delete(TmpPath ++ "/rebar.config.script"),
            erl_tar:create(?APP_NAME ++ ".tar.gz", ["rebar.config.script" | [{FN, TmpPath ++ "/" ++ FN} || FN <- element(2, file:list_dir(TmpPath))]]),
            file:del_dir_r(TmpPath),
            io:format("tar backup: ~s~n", [MainTar]);
        {_, [UpdateTar | _]} ->
            %% 解包
            erl_tar:extract(Backup ++ "/" ++ MainTar, [{cwd, TmpPath}, compressed]),
            erl_tar:extract(TmpPath ++ "/" ++ ?APP_FULL_NAME ++ ".tar.gz", [{cwd, TmpPath ++ "/" ++ ?APP_FULL_NAME}, compressed]),
            erl_tar:extract(Backup ++ "/" ++ UpdateTar, [{cwd, TmpPath ++ "/update"}, compressed]),
            %% 替换文件
            LibPath = TmpPath ++ "/" ++ ?APP_FULL_NAME ++ "/lib/" ++ ?APP_FULL_NAME,
            file:del_dir_r(LibPath),
            file:make_dir(LibPath),
            file:copy(TmpPath ++ "/update/release.erl", TmpPath ++ "/release.erl"),
            file:delete(TmpPath ++ "/update/release.erl"),
            file:delete(TmpPath ++ "/update/rebar.config.script"),
            copy_dir(TmpPath ++ "/update", LibPath),
            file:del_dir_r(TmpPath ++ "/update"),
            {ok, Bin} = file:read_file(TmpPath ++ "/rebar.config.script"),
            ok = file:write_file("rebar.config.script", ["%% tar backup at ", date_str(), $\n, "%% ", MainTar, $\n, Bin]),
            file:delete(TmpPath ++ "/rebar.config.script"),
            %% 重新打包
            erl_tar:create(TmpPath ++ "/" ++ ?APP_FULL_NAME ++ ".tar.gz", [{FN, TmpPath ++ "/" ++ ?APP_FULL_NAME ++ "/" ++ FN} || FN <- element(2, file:list_dir(TmpPath ++ "/" ++ ?APP_FULL_NAME))]),
            file:del_dir_r(TmpPath ++ "/" ++ ?APP_FULL_NAME),
            erl_tar:create(?APP_NAME ++ ".tar.gz", ["rebar.config.script" | [{FN, TmpPath ++ "/" ++ FN} || FN <- element(2, file:list_dir(TmpPath))]]),
            file:del_dir_r(TmpPath),
            io:format("tar backup: ~s with ~s~n", [MainTar, UpdateTar])
    end.

tar_update(Opt) ->
    Profile = proplists:get_value("-profile", Opt, ?RELEASE_TAG),
    io:format("tar update ~s~n", [?APP_FULL_NAME]),
    %% 先编译
    Cmd = "cd .. && \"./rebar3\" as " ++ Profile ++ " compile",
    os:cmd(Cmd),
    EBinPath = "../_build/" ++ Profile ++ "/lib/" ++ ?APP_NAME ++ "/ebin",
    %% 打压缩包
    case filelib:is_dir(EBinPath) of
        true ->
            write_rebar3_config(["tar update at ", date_str(), $\n]),
            erl_tar:create("update.tar.gz", [{"ebin", EBinPath}, {"src", "../src"}, {"priv", "../priv"}, {"include", "../include"}, "release.erl", "rebar.config.script"]),
            io:format("create tar update.tar.gz~n", []),
            backup("update.tar.gz", Opt);
        _ ->
            io:format("tar fail, cmd:~n~s~n", [Cmd])
    end.

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
                "update_" ++ Time ->
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
        "update_" ++ _ ->
            make_backup_tree(T, MainName, [FileName | EBinList], Tree);
        _ ->
            case MainName of
                undefined -> make_backup_tree(T, FileName, [], Tree);
                _ -> make_backup_tree(T, FileName, [], [{MainName, EBinList} | Tree])
            end
    end.

%% 保证同一次操作用相同的时间戳
date_str() ->
    case get(?PD_RELEASE_DATE_STR) of
        undefined ->
            {{Y, M, D}, {H, Min, S}} = erlang:localtime(),
            [date_str_add_zero(Int) || Int <- [Y, M, D, H, Min, S]];
        DateStr ->
            DateStr
    end.

date_str_add_zero(Int) when Int > 9 ->
    integer_to_list(Int);
date_str_add_zero(Int) ->
    [$0, integer_to_list(Int)].

init_node() ->
    ReleaseDir = ?APP_NAME ++ "-" ++ ?APP_VERSION ++ "/releases",
    %% 在vm.args中找到节点信息
    {ok, Bin} = file:read_file(ReleaseDir ++ "/" ++ ?APP_VERSION ++ "/vm.args"),
    {match, ["-name " ++ NodeStr]} = re:run(Bin, "-name .*", [{capture, first, list}]),
    {match, ["-setcookie " ++ CookieStr]} = re:run(Bin, "-setcookie .*", [{capture, first, list}]),
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
    case os:type() of
        {unix, _} ->
            os:cmd("cd " ++ ?APP_FULL_NAME ++ " && nohup ./bin/" ++ ?APP_NAME ++ " console &");
        {win32, _} ->
            spawn(fun() -> os:cmd("cd " ++ ?APP_FULL_NAME ++ " && call ./bin/" ++ ?APP_NAME ++ " console") end)
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

%% 只有两种情况, 第一新部署, 第二替换旧的tar
replace_tar(Opt) ->
    case filelib:is_dir(?APP_FULL_NAME) of
        false ->
            io:format("install tar~n"),
            init_tar(Opt);
        _ ->
            case filelib:is_file("replace_escript_mask") of
                false ->% 使用新的escript
                    io:format("replace escript~n"),
                    ok = erl_tar:extract(?APP_NAME ++ ".tar.gz", [{cwd, "."}, {files, ["release.erl"]}, compressed]),
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
                    SaveDirList = lists:usort(proplists:get_value("-reuse_dir", Opt, []) ++ ?REUSE_DIR),
                    lists:foreach(fun(FN) ->
                        SaveDir = ?APP_FULL_NAME ++ "/" ++ FN,
                        case filelib:is_dir(SaveDir) of
                            true ->
                                io:format("backup dir ~s~n", [SaveDir]),
                                copy_dir(SaveDir, "replace_tar_tmp/" ++ FN);
                            _ ->
                                skip
                        end
                                  end, SaveDirList),
                    file:del_dir_r(?APP_FULL_NAME),
                    ok = erl_tar:extract(?APP_NAME ++ ".tar.gz", [{cwd, "."}, compressed]),
                    file:delete(?APP_NAME ++ ".tar.gz"),
                    io:format("extract: ~s~n", [?APP_NAME ++ ".tar.gz"]),
                    init_tar(Opt)
            end
    end.

init_tar(Opt) ->
    %% 解压启动app
    ok = erl_tar:extract(?APP_FULL_NAME ++ ".tar.gz", [{cwd, ?APP_FULL_NAME}, compressed]),
    %% 复制保存的文件夹
    case filelib:is_dir("replace_tar_tmp") of
        true ->
            SaveDirList = proplists:get_value("-reuse_dir", Opt, ?REUSE_DIR),
            lists:foreach(fun(FN) ->
                case filelib:is_dir("replace_tar_tmp/" ++ FN) of
                    true ->
                        io:format("recover dir ~s~n", [?APP_FULL_NAME ++ "/" ++ FN]),
                        copy_dir("replace_tar_tmp/" ++ FN, ?APP_FULL_NAME ++ "/" ++ FN);
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
    %% 清理文件
    file:delete(?APP_FULL_NAME ++ ".tar.gz").

update_app(Opt) ->
    update_app("update.tar.gz", Opt).

update_app(Tar, Opt) ->
    case filelib:is_file("replace_escript_mask") of
        false ->% 使用新的escript
            io:format("replace escript~n"),
            ok = erl_tar:extract(Tar, [{cwd, "."}, {files, ["release.erl"]}, compressed]),
            file:write_file("replace_escript_mask", []),
            escript:start();
        _ ->
            file:delete("replace_escript_mask"),
            {M, F, A} = get_mfa(Opt, ?UPDATE_MFA),
            %% 解压
            ok = erl_tar:extract(Tar, [{cwd, "update"}, compressed]),
            io:format("update: ~s~n", [Tar]),
            %% 移动文件到lib对应的app
            AppPath = ?APP_FULL_NAME ++ "/lib/" ++ ?APP_FULL_NAME,
            file:del_dir_r(AppPath),
            file:make_dir(AppPath),
            file:delete("update/release.erl"),
            file:copy("update/rebar.config.script", "rebar.config.script"),
            file:delete("update/rebar.config.script"),
            copy_dir("update", AppPath),
            %% 删除更新相关的文件
            file:del_dir_r("update"),
            file:delete(Tar),
            Node = init_node(),
            %% 游戏节点更新或者开启
            case net_kernel:connect_node(Node) of
                true ->
                    io:format("~w~n", [rpc:call(Node, M, F, A)]);
                _ ->
                    io:format("node not start~n")
            end
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
        case filelib:is_dir(FromFilePath) of
            true ->
                file:del_dir_r(CopyFilePath),
                file:make_dir(CopyFilePath),
                copy_dir(FromFilePath, CopyFilePath),
                Acc;
            _ ->
                file:copy(FromFilePath, CopyFilePath),
                Acc
        end end, {From, To}, CopyList).

%% 保留打包时的信息
write_rebar3_config(Head) ->
    {ok, Bin} = file:read_file("../rebar.config.script"),
    ok = file:write_file("rebar.config.script", [Head, Bin]).

%% 命令行支持中文好难弄, 写着凑合
%% tar: 打包一个版本
%% tar_backup:  打包一个backup树的分支
%% tar_update:    打包app相关文件, 用于更新
%% backup_tree: 显示backup树
%% replace_tar: 部署或更新一个tar
%% restart: 重启
%% stop: 关闭
%% update_app: 更新上面打出来的update包, 仅替换文件
usage() ->
    io:format("
release tool
use:    escript release.erl [cmd] [-option value]

cmd:
tar:    create a " ++ ?APP_NAME ++ ".tar.gz
    -profile [tag], tag define in rebar.config.script => profiles, default " ++ ?RELEASE_TAG ++ "
    -backup [dir], backup tar to dir like " ++ ?APP_NAME ++ "_yyyymmddhhmmss.tar.gz
    
tar_backup:   create a " ++ ?APP_NAME ++ ".tar.gz
        read app.tar.gz and update.tar.gz from backup dir
        pack newest update.tar.gz into app.tar.gz
    -backup [dir], backup tar to dir like " ++ ?APP_NAME ++ "_yyyymmddhhmmss.tar.gz
    -date [yyyymmddhhmmss]
    
tar_update:   tar app for update release
    -profile [tag], tag define in rebar.config.script => profiles, default " ++ ?RELEASE_TAG ++ "
    -backup [dir], backup tar to dir like update_yyyymmddhhmmss.tar.gz
    
backup_tree:    show 'app tar' and 'update tar' time relation
    -backup [dir], backup dir
    
replace_tar:    update 'app tar', stop -> update/install -> start
                install if no dir(meaning exist an app) in the path
                orelse, update
    -mfa [{M,F,A}], if need stop, default " ++ io_lib:format("~w", [?STOP_MFA]) ++ "
    -wait [time], start -> wait X ms -> update
    -reuse_dir [dir1[ dir2]], reuse these dir when replace, default " ++ string:join(?REUSE_DIR, " ") ++ "
    
restart:    restart, stop -> start

stop:   stop, rpc call mfa to stop the node
    -mfa [{M,F,A}], default " ++ io_lib:format("~w", [?STOP_MFA]) ++ "
    
update_app:    update 'app's ebin/src/include/priv and rpc call mfa to stop the node or start node
                this cmd just replace file, if it effect boot, rel, .etc, you have to tar it again
    -mfa [{M,F,A}], default " ++ io_lib:format("~w", [?STOP_MFA]) ++ "
").