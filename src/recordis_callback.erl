-module(recordis_callback).
-author("yimo").

-export([before_new/1, after_new/1]).
-export([before_update/1, after_update/1]).
-export([before_delete/1, after_delete/1]).


before_new(Record1) ->
    F = find_callback(Record1, ?FUNCTION_NAME),
    Record2 = call(F, Record1),
    case same_record(Record1, Record2) of
        true -> Record2;
        _ -> Record1
    end.

before_update(Record1) ->
    F = find_callback(Record1, ?FUNCTION_NAME),
    Record2 = call(F, Record1),
    case same_record(Record1, Record2) of
        true -> Record2;
        _ -> Record1
    end.

before_delete(Record1) ->
    F = find_callback(Record1, ?FUNCTION_NAME),
    Record2 = call(F, Record1),
    case same_record(Record1, Record2) of
        true -> Record2;
        _ -> Record1
    end.

after_new(Record) ->
    F = find_callback(Record, ?FUNCTION_NAME),
    call(F, Record).
after_update(Record) ->
    F = find_callback(Record, ?FUNCTION_NAME),
    call(F, Record).
after_delete(Record) ->
    F = find_callback(Record, ?FUNCTION_NAME),
    call(F, Record).



find_callback(Record, Function) ->
    proplists:get_value(Function, recordis_utils:obj_callback(Record)).

call(undefined, Record) -> Record;
call({Module, Function}, Record) -> apply(Module, Function, [Record]);
call(Function, Record) -> Function(Record).

same_record(R1, R2) ->
    erlang:element(1, R1) =:= erlang:element(1, R2).