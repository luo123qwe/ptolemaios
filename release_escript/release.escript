#!/usr/bin/env escript

main(_) ->
    {_IsMain, _NewDir, VmArgs, _Config} = update_main(),
    %% 到这一步我们认为tar是没问题
    {_, NodeStr} = lists:keyfind("-name", 1, VmArgs),
    Node = list_to_atom(NodeStr),
    {_, CookieStr} = lists:keyfind("-cookie", 1, VmArgs),
    Cookie = list_to_atom(CookieStr),
    % 开启本节点
    net_kernel:start([atom_to_list("release_" ++ integer_to_list(erlang:system_time(second)) ++ "@127.0.0.1")]),
    erlang:set_cookie(Node, Cookie),
    %% 游戏节点更新或者开启
    case net_kernel:connect_node(Node) of
        true ->
            io:format("~w~n", [rpc:call(Node, fix_hot, fix, [])]);
        _ ->
            case os:type() of
                {unix, _} ->
                    os:cmd("cd main && nohup ./bin/test console &");
                {win32, _} ->
                    os:cmd("cd main&start /c ./bin/test console")
            end,
            io:format("start console")
    end.

update_main() ->
    case file:list_dir(".") of
        {ok, [_, _, _] = ListDir} ->
            %% 第一个tar
            [NewTar] = ListDir -- ["vm.args.src", "release.escript"],
            NewDir = filename:rootname(NewTar, ".tar.gz"),
            {VmArgs, Config} = extract_tag(NewDir),
            %% 必须要有erts
            case lists:any(fun(N) ->
                case N of
                    "erts" ++ _ -> true;
                    _ -> false
                end end, element(2, file:list_dir(NewDir)))
            of
                true -> ok;
                _ ->
                    delete_tar(NewDir),
                    throw(no_erts)
            end,
            %% 好像没有copy文件夹的api
            erl_tar:extract(NewTar, [{cwd, "main"}, compressed]),
            {true, NewDir, VmArgs, Config};
        {ok, ListDir} when length(ListDir) > 3 ->
            %% 获取最新的压缩包
            NewTar =
                lists:foldl(fun(Name, Acc) ->
                    case filelib:is_dir(Name) of
                        true ->% 文件夹
                            Acc;
                        false ->% 压缩包
                            case filelib:is_dir(filename:rootname(Name, ".tar.gz")) of
                                true -> Acc;
                                false -> Name
                            end
                    end
                            end, undefined, ListDir),
            case NewTar of
                undefined -> throw(no_new_tar);
                _ -> ok
            end,
            NewDir = filename:rootname(NewTar, ".tar.gz"),
            {VmArgs, Config} = extract_tag(NewDir),
            %% 更新文件
            copy_update_file(NewDir, "main"),
            {true, NewDir, VmArgs, Config}
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
    file:write_file(NewDir ++ "vm.args", VmArgsIOData),
    {ok, Config} = file:script(ReleaseDir ++ "/config.args.src"),
    ConfigIOData = io_lib:format("~p", [Config]),
    file:write_file(NewDir ++ "config.args.src", ConfigIOData),
    {VmArgs, Config}.

delete_tar(NewDir) ->
    file:del_dir_r(NewDir),
    file:delete(NewDir ++ ".tar.gz").


copy_update_file(From, To) ->
    lists:foldl(fun(N, {F, T} = Acc) ->
        FromFilePath = F ++ "/" ++ N,
        CopyFilePath = T ++ "/" ++ N,
        case filelib:is_dir(N) of
            true ->
                file:del_dir_r(CopyFilePath),
                file:make_dir(CopyFilePath),
                copy_update_file(FromFilePath, CopyFilePath),
                Acc;
            _ ->
                file:copy(FromFilePath, CopyFilePath),
                Acc
        end end, {From, To}, ["bin", "lib", "release"]).