-module(recordis_index).
-author("yimo").

-export([select/4]).
-export([create/2, reindex/2, drop/2]).
-export([upsert/1, delete/1]).
-export([string_hash/1]).

-include("recordis.hrl").

%% int索引     使用sorted_set实现
%% index_string 索引 使用Karp–Rabin算法把string转成int实现
select(Record, Column, Left, Right) ->
    L = to_sorted_set_key(Left),
    R = to_sorted_set_key(Right),
    Index = recordis_concat:index(Record, Column),
    recordis_sorted_set:get_score(Index, L, R).

upsert(Record) ->
    Indexes = recordis_utils:index(Record),
    lists:map(fun(Column) -> upsert(Record, Column) end, Indexes).

upsert(Record, Column) ->
    Index = recordis_concat:index(Record, Column),
    Value = to_sorted_set_key(recordis_utils:get_value(Column, Record)),
    Pk = recordis_utils:pk(Record),
    recordis_sorted_set:set(Index, [{Value, Pk}]).

delete(Record) ->
    Indexes = recordis_utils:index(Record),
    lists:map(fun(Column) -> delete(Record, Column) end, Indexes).

delete(Record, Column) ->
    Index = recordis_concat:index(Record, Column),
    Pk = recordis_utils:pk(Record),
    recordis_sorted_set:remove(Index, Pk).

create(Record, Column) ->
    Indexes = recordis_utils:index(Record),
    true = lists:member(Column, Indexes),
    throw(todo_error).

reindex(Record, Column) ->
    drop(Record, Column),
    create(Record, Column).

drop(Record, Column) ->
    Indexes = recordis_utils:index(Record),
    false = lists:member(Column, Indexes),
    Index = recordis_concat:index(Record, Column),
    recordis_key:delete(Index).

to_sorted_set_key(N) when is_number(N) -> N;
to_sorted_set_key(Binary) when is_binary(Binary) -> string_hash(Binary).

string_hash(Binary) ->
    karp_rabin(Binary, 0).
karp_rabin(<<Char:8, Rest/binary>>, Acc) ->
    karp_rabin(Rest, Acc * 37 + char_to_int(Char));
karp_rabin(<<>>, Acc) ->
    Acc.
char_to_int(Char) when $0 =< Char andalso Char =< $9 ->
    Char - 48;
char_to_int(Char) when $A =< Char andalso Char =< $Z ->
    Char - 55.