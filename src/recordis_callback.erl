-module(recordis_callback).
-author("yimo").

-include_lib("recordis.hrl").

-export([before_new/1, after_new/1]).
-export([before_update/1, after_update/1]).
-export([before_delete/1, after_delete/1]).


before_new(Record) ->
    call(find_callback(Record, ?FUNCTION_NAME), Record).
before_update(Record) ->
    call(find_callback(Record, ?FUNCTION_NAME), Record).
before_delete(Record) ->
    call(find_callback(Record, ?FUNCTION_NAME), Record).
after_new(Record) ->
    call(find_callback(Record, ?FUNCTION_NAME), Record).
after_update(Record) ->
    call(find_callback(Record, ?FUNCTION_NAME), Record).
after_delete(Record) ->
    call(find_callback(Record, ?FUNCTION_NAME), Record).


find_callback(Record, before_new) ->
    Callback = recordis_utils:callback(Record),
    Callback#recordis_callback.before_new;
find_callback(Record, after_new) ->
    Callback = recordis_utils:callback(Record),
    Callback#recordis_callback.after_new;
find_callback(Record, before_update) ->
    Callback = recordis_utils:callback(Record),
    Callback#recordis_callback.before_update;
find_callback(Record, after_update) ->
    Callback = recordis_utils:callback(Record),
    Callback#recordis_callback.after_update;
find_callback(Record, before_delete) ->
    Callback = recordis_utils:callback(Record),
    Callback#recordis_callback.before_delete;
find_callback(Record, after_delete) ->
    Callback = recordis_utils:callback(Record),
    Callback#recordis_callback.after_delete.


call([{Module, Function} | Rest], Record) ->
    call(Rest, choice_record(Record, apply(Module, Function, [Record])));
call([Function | Rest], Record) ->
    call(Rest, choice_record(Record, Function(Record)));
call([], Record) ->
    Record.


choice_record(OriginRecord, NewRecord) ->
    case erlang:element(1, OriginRecord) =:= erlang:element(1, NewRecord) of
        true -> NewRecord;
        false -> OriginRecord
    end.
