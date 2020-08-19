-module(recordis).
-author("yimo").

-include("recordis.hrl").

-export([use/1, use/2]).
-export([new/1, delete/1, update/1]).
-export([one/1, one/2, all/2]).

-define(SpinTimeout, 5).
-define(KeyTimeout, 30).

use(Client) ->
    put(recordis, Client).

use({Module, Q}, {Module, QP}) ->
    put(recordis, {{Module, Q}, {Module, QP}}).

new(Record) ->
    transaction(Record, fun() -> recordis_ctrl:new(Record) end).

update(Record) ->
    transaction(Record, fun() -> recordis_ctrl:update(Record) end).

delete(Record) ->
    transaction(Record, fun() -> recordis_ctrl:delete(Record) end).

one(Record) ->
    recordis_ctrl:get(Record).

one(Record, Keys) ->
    recordis_ctrl:get(Record, Keys).

all(Record, Where) ->
    recordis_find:all(Record, Where).

transaction(Record, Fun) ->
    try
        recordis_lock:acquire(Record, ?SpinTimeout, ?KeyTimeout),
        Fun()
    catch
        throw:timeout_error -> recordis_lock:release(Record);
        _:_ -> ignore
    end.