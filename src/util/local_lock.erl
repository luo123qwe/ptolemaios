%%%-------------------------------------------------------------------
%%% @author dominic
%%% @doc 单节点的锁
%%%
%%% 通过ets的xxx_element函数实现简单的悲观锁
%%% @end
%%%-------------------------------------------------------------------
-module(local_lock).
-author("dominic").

-include("util.hrl").

%% API
-export([
    init_ets/0,
    get_owner/1,
    lock/1,
    lock/2,
    lock/3,
    lock/4,
    release/1,
    release/2,
    force_lock/1,
    force_lock/2,
    force_release/1,
    force_release/2
]).

%% @doc 初始化ets
-spec init_ets() -> ok.
init_ets() ->
    ?ETS_LOCAL_LOCK = ets:new(?ETS_LOCAL_LOCK, [public, named_table, {keypos, #local_lock.key}]),
    ok.

%% @doc 获取锁的拥有者
-spec get_owner(term()) -> undefined|any().
get_owner(Key) ->
    try ets:lookup_element(?ETS_LOCAL_LOCK, Key, #local_lock.owner)
    catch
        _:_ -> undefined
    end.

%% @equiv lock(Key, self())
lock(Key) ->
    lock(Key, self()).

%% @doc 获取锁, 成功时返回lock, 已获取锁时依然返回fail
-spec lock(term(), term()) -> fail|lock.
lock(Key, Owner) ->
    case ets:update_counter(?ETS_LOCAL_LOCK, Key, {#local_lock.lock, 1}, #local_lock{key = Key, owner = Owner}) of
        1 ->% lock
            lock;
        _ ->
            fail
    end.


%% @equiv lock(Key, self(), Interval, Times)
lock(Key, Interval, Times) ->
    lock(Key, self(), Interval, Times).

%% @doc 尝试N次间隔M毫秒获取锁, 成功时返回lock, 已获取锁时依然返回fail
-spec lock(term(), term(), non_neg_integer(), non_neg_integer()) -> fail|lock.
lock(_Key, _Owner, _Interval, Times) when Times =< 0 ->
    fail;
lock(Key, Owner, Interval, Times) ->
    case ets:update_counter(?ETS_LOCAL_LOCK, Key, {#local_lock.lock, 1}, #local_lock{key = Key, owner = Owner}) of
        1 ->% lock
            lock;
        _ ->
            timer:sleep(Interval),
            lock(Key, Owner, Interval, Times - 1)
    end.


%% @equiv release(Key, self())
release(Key) ->
    release(Key, self()).

%% @doc 释放锁, 如果不是owner会报错
-spec release(term(), term()) -> release.
release(Key, Owner) ->
    case get_owner(Key) of
        undefined ->
            release;
        Owner ->
            ets:delete(?ETS_LOCAL_LOCK, Key),
            release;
        false ->
            erlang:error(not_owner)
    end.


%% @equiv force_lock(Key, self())
force_lock(Key) ->
    force_lock(Key, self()).

%% @doc 强制获取锁, 成功时返回lock或force_lock, 已获取锁时依然会重新获取锁
%%
%% 如果出现以下并发情况则返回fail, A删除锁 -> B增加锁 -> A增加锁失败
%%
%% 以下情况并发不能保证锁功能正确执行, A删除锁 -> A添加锁 -> A认为成功锁 -> B删除锁 -> B添加锁 -> B认为成功锁
-spec force_lock(term(), term()) -> lock|force_lock|fail.
force_lock(Key, Owner) ->
    case ets:update_counter(?ETS_LOCAL_LOCK, Key, {#local_lock.lock, 1}, #local_lock{key = Key, owner = Owner}) of
        1 ->% lock
            lock;
        _ ->
            ets:delete(?ETS_LOCAL_LOCK, Key),
            case ets:insert_new(Key, #local_lock{key = Key, owner = Owner, lock = 1}) of
                true -> force_lock;
                false -> fail
            end
    end.


%% @equiv force_release(Key, self())
force_release(Key) ->
    force_release(Key, self()).

%% @doc 强制释放锁
-spec force_release(term(), term()) -> force_release|release.
force_release(Key, Owner) ->
    case get_owner(Key) of
        undefined ->
            release;
        Owner ->
            ets:delete(?ETS_LOCAL_LOCK, Key),
            release;
        _ ->
            ets:delete(?ETS_LOCAL_LOCK, Key),
            force_release
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

base_test_() ->
    [
        {setup,
            fun() -> local_lock:init_ets() end,
            fun(_) -> ets:delete(?ETS_LOCAL_LOCK) end,
            fun(_X) ->
                [
                    ?_assertEqual(lock, local_lock:lock(a)),
                    ?_assertEqual(self(), local_lock:get_owner(a)),
                    ?_assertEqual(fail, local_lock:lock(a)),
                    ?_assertEqual(release, local_lock:release(a)),
                    ?_assertError(not_owner, local_lock:release(a)),
                    ?_test(spawn(fun() -> local_lock:lock(b), timer:sleep(10 * 1000) end)),
                    ?_assertEqual(fail, local_lock:lock(b)),
                    ?_assertError(not_owner, local_lock:release(b)),
                    ?_assertEqual(force_release, local_lock:force_release(b)),
                    ?_assertEqual(undefined, local_lock:get_owner(b)),
                    ?_assertEqual(lock, local_lock:lock(b)),
                    ?_assertEqual(self(), local_lock:get_owner(b))
                ]
            end}
    ].

-endif.