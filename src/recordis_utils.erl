-module(recordis_utils).
-author("yimo").

-define(Delimiter, <<":">>).

%% API
-export([
    primary_key/1,
    obj_type/1,
    obj_column/1,
    obj_primary_key/1,
    obj_relation/1,
    key_concat/1,
    obj_value/2,
    enumerate/1,
    flatten_map/1,
    yes_ok/1,
    is_s_key/1,
    all_keys/1
]).


primary_key(Obj) ->
    key_concat([obj_type(Obj), obj_primary_key(Obj)]).

obj_type(Obj) ->
    atom_to_binary(erlang:element(1, Obj), utf8).

obj_column(Obj) ->
    erlang:element(2, Obj).
obj_relation(Obj) ->
    erlang:element(3, Obj).
obj_primary_key(Obj) ->
    case erlang:element(4, Obj) of
        undefine -> throw(miss_primary_key_error);
        Pk -> Pk
    end.

%% the primary key index is 1
obj_value(Index, Obj) ->
    erlang:element(Index + 3, Obj).

key_concat(Ls) ->
    list_to_binary(lists:join(?Delimiter, Ls)).

enumerate(Ls) ->
    lists:zip(lists:seq(1, length(Ls)), Ls).

flatten_map(Map) ->
    maps:fold(fun(K, V, A) -> [K, V | A] end, [], Map).


yes_ok(Ls) ->
    true = lists:all(fun({ok, _}) -> true;(_) -> false end, Ls).


all_keys(Record) ->
    [primary_key(Record) | column_keys(Record)] ++ link_keys(Record).

link_keys(_Record) ->
    [].

column_keys(Record) ->
    Type = obj_type(Record),
    Pk = obj_primary_key(Record),
    F = fun({K, T}, Acc) ->
        case is_s_key(T) of
            true -> [key_concat([Type, atom_to_binary(K, utf8), Pk]) | Acc];
            false -> Acc
        end
        end,
    lists:foldl(F, [], obj_column(Record)).


is_s_key(hash) -> true;
is_s_key(set) -> true;
is_s_key(sorted_set) -> true;
is_s_key(_) -> false.
