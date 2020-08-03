-module(recordis_lock).
-author("yimo").


-define(Second, 1000).
%% API
-export([acquire/3]).
-export([release/1]).

%% 默认只提供最简单的一种锁
acquire(RecordWithPk, SpinTimeout, KeyTimeout) ->
    spin(fun() -> get_lock(RecordWithPk, KeyTimeout) end, SpinTimeout * ?Second).

get_lock(RecordWithPk, Timeout) ->
    Lock = recordis_concat:key_to_lock(recordis_utils:primary_key(RecordWithPk)),
    recordis_redis:q(recordis_key:set_nx(Lock, 1, Timeout)).

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
