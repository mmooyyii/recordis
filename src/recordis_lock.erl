-module(recordis_lock).
-author("yimo").


-define(Second, 1000).
%% API
-export([acquire/1, acquire/2]).
-export([spin_acquire/2, spin_acquire/3]).
-export([release/1]).


%% 默认提供最简单的一种锁，如有需要可以通过{...}接口自己实现
acquire(RecordWithPk) ->
    Lock = recordis_concat:key_to_lock(recordis_utils:primary_key(RecordWithPk)),
    recordis_redis:q(recordis_key:set_nx(Lock, 1)).

acquire(RecordWithPk, Timeout) ->
    Lock = recordis_concat:key_to_lock(recordis_utils:primary_key(RecordWithPk)),
    recordis_redis:q(recordis_key:set_nx(Lock, 1, Timeout)).

spin_acquire(RecordWithPk, SpinTimeout) ->
    spin(fun() -> acquire(RecordWithPk) end, SpinTimeout * ?Second).

spin_acquire(RecordWithPk, SpinTimeout, KeyTimeout) ->
    spin(fun() -> acquire(RecordWithPk, KeyTimeout) end, SpinTimeout * ?Second).

release(RecordWithPk) ->
    Lock = recordis_concat:key_to_lock(recordis_utils:primary_key(RecordWithPk)),
    recordis_redis:q(recordis_key:delete(Lock)).

spin(_Todo, Timeout) when Timeout =< 0 ->
    false;
spin(Todo, Timeout) ->
    case Todo() of
        true -> true;
        false -> timer:sleep(100), spin(Todo, Timeout)
    end.


