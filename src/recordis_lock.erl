-module(recordis_lock).
-author("yimo").

%% API
-export([acquire/1, acquire/2]).
-export([spin_acquire/2, spin_acquire/3]).
-export([release/1]).

%% 默认提供最简单的一种锁，如有需要可以通过{...}接口自己实现
-spec acquire(tuple()) -> no_return() | throw_error.
acquire(RecordWithPk) ->
    Lock = recordis_utils:key_to_lock(recordis_utils:primary_key(RecordWithPk)),
    recordis_redis:q(recordis_key:set_nx(Lock, 1)).

-spec acquire(binary(), integer()) -> no_return() | throw_error.
acquire(RecordWithPk, Timeout) ->
    Lock = recordis_utils:key_to_lock(recordis_utils:primary_key(RecordWithPk)),
    recordis_redis:q(recordis_key:set_nx(Lock, 1, Timeout)).

-spec spin_acquire(binary(), integer()) -> no_return() | throw_error.
spin_acquire(RecordWithPk, SpinTimeout) ->
    spin(fun() -> acquire(RecordWithPk) end, SpinTimeout * 1000).

-spec spin_acquire(binary(), integer(), integer()) -> no_return() | throw_error.
spin_acquire(RecordWithPk, SpinTimeout, KeyTimeout) ->
    spin(fun() -> acquire(RecordWithPk, KeyTimeout) end, SpinTimeout * 1000).

-spec release(binary()) -> no_return().
release(RecordWithPk) ->
    Lock = recordis_utils:key_to_lock(recordis_utils:primary_key(RecordWithPk)),
    recordis_redis:q(recordis_key:delete(Lock)).

spin(_Todo, Timeout) when Timeout =< 0 ->
    false;
spin(Todo, Timeout) when Timeout =< 0 ->
    case Todo() of
        true -> true;
        false -> timer:sleep(100), spin(Todo, Timeout)
    end.


