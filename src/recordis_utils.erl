-module(recordis_utils).
-author("yimo").

-define(Delimiter, <<":">>).

%% API
-export([
    primary_key/1,
    obj_type/1,
    obj_column/1,
    obj_column_key/1,
    obj_primary_key/1,
    obj_relation/1,
    key_concat/1,
    obj_value/2,
    enumerate/1,
    enumerate_r/1,
    flatten_map/1,
    un_flatten_map/1,
    is_s_key/1,
    all_keys/1,
    soft_delete_key/1,
    obj_set_value/3,
    without_value/2,
    lists_div/2
]).


primary_key(Obj) ->
    key_concat([obj_type(Obj), obj_primary_key(Obj)]).

obj_type(Obj) ->
    atom_to_binary(erlang:element(1, Obj), utf8).

obj_column(Obj) ->
    erlang:element(2, Obj).

obj_column_key(Obj) ->
    lists:map(fun({Key, _}) -> Key end, erlang:element(2, Obj)).

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
obj_set_value(Index, V, Obj) ->
    erlang:setelement(Index + 3, Obj, V).

key_concat(Ls) ->
    list_to_binary(lists:join(?Delimiter, lists:map(fun to_binary/1, Ls))).

enumerate(Ls) ->
    lists:zip(lists:seq(1, length(Ls)), Ls).
enumerate_r(Ls) ->
    lists:zip(Ls, lists:seq(1, length(Ls))).


flatten_map(Map) ->
    maps:fold(fun(K, V, A) -> [K, V | A] end, [], Map).


un_flatten_map([undefined]) ->
    #{};
un_flatten_map(List) ->
    un_flatten_map(List, #{}).
un_flatten_map([K, V | R], Acc) ->
    un_flatten_map(R, Acc#{K => V});
un_flatten_map([], Acc) ->
    Acc.


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


soft_delete_key(Key) ->
    <<Key/binary, ":d">>.

to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
to_binary(Binary) -> Binary.

without_value(Map, Val) ->
    maps:fold(fun(_K, V, Acc) when V =:= Val -> Acc;(K, V, Acc) -> Acc#{K => V} end, #{}, Map).

lists_div(N, List) when (length(List) rem N) =:= 0 ->
    NLen = length(List) div N,
    p_list_div(0, NLen, [[]], List).

p_list_div(NLen, NLen, [A | Acc], []) ->
    lists:reverse([lists:reverse(A) | Acc]);
p_list_div(NLen, NLen, [A | Acc], List) ->
    p_list_div(0, NLen, [[], lists:reverse(A) | Acc], List);
p_list_div(N, NLen, [A | Acc], [E | Rest]) ->
    p_list_div(N + 1, NLen, [[E | A] | Acc], Rest).
