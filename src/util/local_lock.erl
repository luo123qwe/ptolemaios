%%%-------------------------------------------------------------------
%%% @author dominic
%%% @doc
%%% 单节点的锁
%%% @end
%%%-------------------------------------------------------------------
-module(local_lock).
-author("dominic").

-include("util.hrl").

%% API
-export([
    init_ets/0,
    is_lock/1,
    is_lock/2,
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

%% @equiv is_lock(Key, self())
is_lock(Key) ->
    is_lock(Key, self()).

%% @doc 是否已经上锁
-spec is_lock(term(), term()) -> boolean().
is_lock(Key, Owner) ->
    (catch ets:lookup_element(?ETS_LOCAL_LOCK, Key, #local_lock.owner)) == Owner.

%% @equiv lock(Key, self())
lock(Key) ->
    lock(Key, self()).

%% @doc 获取锁, 成功时返回lock, 
%% 已获取锁时依然返回fail
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

%% @doc 尝试N次间隔M毫秒获取锁, 成功时返回lock, 
%%%% 已获取锁时依然返回fail
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

-spec release(term(), term()) -> release.
release(Key, Owner) ->
    case is_lock(Key, Owner) of
        true ->
            ets:delete(?ETS_LOCAL_LOCK, Key),
            release;
        false ->
            erlang:error(not_owner)
    end.


%% @equiv force_lock(Key, self())
force_lock(Key) ->
    force_lock(Key, self()).

%% @doc 强制获取锁, 成功时返回lock或force_lock, 
%%%% 已获取锁时会重新获取锁
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
    case is_lock(Key, Owner) of
        true ->
            ets:delete(?ETS_LOCAL_LOCK, Key),
            release;
        false ->
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
                    ?_assertEqual(true, local_lock:is_lock(a)),
                    ?_assertEqual(fail, local_lock:lock(a)),
                    ?_assertEqual(release, local_lock:release(a)),
                    ?_assertError(not_owner, local_lock:release(a)),
                    ?_test(spawn(fun() -> local_lock:lock(b), timer:sleep(10 * 1000) end)),
                    ?_assertEqual(fail, local_lock:lock(b)),
                    ?_assertError(not_owner, local_lock:release(b)),
                    ?_assertEqual(force_release, local_lock:force_release(b)),
                    ?_assertEqual(false, local_lock:is_lock(b)),
                    ?_assertEqual(lock, local_lock:lock(b)),
                    ?_assertEqual(true, local_lock:is_lock(b))
                ]
            end}
    ].

-endif.