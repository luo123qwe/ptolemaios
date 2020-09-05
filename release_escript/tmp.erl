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

main(["update"]) ->
    update_main(),
    %% todo 还要有一个脚本搞node相关的
    Node =,
    case net_adm:ping(Node) of
        pong ->
            rpc:call(Node, fix_hot, fix, []);
        pang ->
            os:cmd("bin/test console")
    end.

update_main() ->
    case file:list_dir(".") of
        [NewTar] ->
            %% 第一个tar
            NewDir = NewTar -- "tar.gz",
            case catch erl_tar:extract(NewTar, [{cwd, NewDir}, compressed]) of
                ok -> ok;
                _ ->
                    file:delete(NewTar),
                    throw(bad_tar)
            end,
            %% 必须要有erts
            case lists:any(fun(N) ->
                case N of
                    "erts" ++ _ -> true;
                    _ -> false
                end end, file:list_dir(NewDir))
            of
                true -> ok;
                _ ->
                    file:delete(NewTar),
                    throw(no_erts)
            end,
            %% 好像没有copy文件夹的api,
            erl_tar:extract(NewTar, [{cwd, "main"}, compressed]);
        ListDir ->
            %% 获取最新的压缩包
            NewTar =
                lists:foldl(fun(Name, Acc) ->
                    case filelib:is_dir(Name) of
                        true ->% 文件夹
                            Acc;
                        false ->% 压缩包
                            case filelib:is_dir(Name -- "tar.gz") of
                                true -> Acc;
                                false -> Name
                            end
                    end
                            end, undefined, ListDir),
            case NewTar of
                undefined -> throw(no_new_tar);
                _ -> ok
            end,
            NewDir = NewTar -- "tar.gz",
            case catch erl_tar:extract(NewTar, [{cwd, NewDir}, compressed]) of
                ok -> ok;
                _ ->
                    file:delete(NewTar),
                    throw(bad_tar)
            end,
            %% 更新文件
            copy_update_file(NewDir, "main")
    end.


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