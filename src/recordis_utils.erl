-module(recordis_utils).
-author("yimo").

-define(Delimiter, <<":">>).


%% API
-export([primary_key/1, key_concat/1, flatten_map/1, un_flatten_map/1, is_s_key/1]).
-export([all_keys/1, set_value/3, set_pk/2]).
-export([type/1, column/1, column_key/1, pk/1, callback/1, link/1, get_value/2, value/2, index/1]).
-export([enumerate/1, enumerate_r/1]).
-export([without_value/2, lists_div/2]).

primary_key(Record) -> key_concat([type(Record), pk(Record)]).
column_key(Record) -> lists:map(fun({Key, _}) -> Key end, erlang:element(2, Record)).
type(Record) -> atom_to_binary(erlang:element(1, Record), utf8).
column(Record) -> erlang:element(2, Record).
link(Record) -> erlang:element(3, Record).
callback(Record) -> erlang:element(4, Record).
index(Record) -> erlang:element(5, Record).


pk(Record) ->
    case erlang:element(6, Record) of
        undefine -> throw(miss_primary_key_error);
        Pk -> Pk
    end.

%% slow?
get_value(Key, Record) ->
    Column = column(Record),
    Index = list_find(Key, [C || {C, _} <- Column]),
    value(Index, Record).
%% the primary key index is 1
value(Index, Record) ->
    erlang:element(Index + 5, Record).
set_pk(Value, Record) ->
    erlang:setelement(6, Record, Value).
set_value(Index, V, Record) ->
    erlang:setelement(Index + 5, Record, V).

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
    Type = type(Record),
    Pk = pk(Record),
    F = fun({K, T}, Acc) ->
        case is_s_key(T) of
            true -> [key_concat([Type, atom_to_binary(K, utf8), Pk]) | Acc];
            false -> Acc
        end
        end,
    lists:foldl(F, [], column(Record)).

is_s_key(hash) -> true;
is_s_key(set) -> true;
is_s_key(sorted_set) -> true;
is_s_key(_) -> false.

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

list_find(Key, Ls) -> list_find(Key, Ls, 1).

list_find(_Key, [], _Index) -> throw(value_error);
list_find(Key, [Key | _], Index) -> Index;
list_find(Key, [_ | Rest], Index) -> list_find(Key, Rest, Index + 1).
