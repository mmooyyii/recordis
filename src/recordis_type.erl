-module(recordis_type).
-author("yimo").

%% API
-export([redis/2, erl/2, check/2]).


redis(index_string, String) when is_binary(String) -> String;
redis(string, String) when is_binary(String) -> String;
redis(int, Int) when is_integer(Int) -> integer_to_binary(Int);
redis(float, Float) when is_float(Float) -> float_to_binary(Float);
redis(term, Term) -> term_to_binary(Term);

redis(hash, Map) -> maps:fold(fun(K, V, A) -> [K, V | A] end, [], Map);
redis(set, Set) -> sets:to_list(Set);
redis(sorted_set, []) -> [];
redis(sorted_set, [{_, _} | _] = PropList) -> lists:foldl(fun({K, V}, A) -> [K, V | A] end, [], PropList);
redis(_, V) when is_binary(V) -> V.

erl(index_string, String) when is_binary(String) -> String;
erl(string, String) when is_binary(String) -> String;
erl(int, Int) when is_binary(Int) -> binary_to_integer(Int);
erl(float, Float) when is_binary(Float) -> binary_to_float(Float);
erl(term, Term) -> binary_to_term(Term);

erl(hash, V) -> hash_to_map(V, #{});
erl(set, List) -> sets:from_list(List);
erl(sorted_set, V) -> sorted_set_to_proplist(V, []);
erl(_, V) -> V.

check(_, undefined) -> true;
check(index_string, String)
    when is_binary(String) andalso size(String) =< 10 -> valid_index_string(String);
check(string, String) when is_binary(String) -> true;
check(int, Int) when is_integer(Int) -> true;
check(float, Float) when is_float(Float) -> true;
check(term, _) -> true;
check(hash, Map) when is_map(Map) -> true;
check(set, Set) -> sets:is_set(Set);
check(sorted_set, []) -> true;
check(sorted_set, [{_, _} | _]) -> true;
check(_, _) -> false.

hash_to_map([K, V | Rest], Acc) ->
    hash_to_map(Rest, Acc#{K => V});
hash_to_map([], Acc) ->
    Acc.

sorted_set_to_proplist([V, Score | Rest], Acc) ->
    sorted_set_to_proplist(Rest, [{Score, V} | Acc]);
sorted_set_to_proplist([], Acc) ->
    lists:reverse(Acc).

valid_index_string(<<Char:8, Rest/binary>>)
    when ($0 =< Char andalso Char =< $9) orelse ($A =< Char andalso Char =< $Z) ->
    valid_index_string(Rest);
valid_index_string(<<>>) -> true;
valid_index_string(_) ->
    false.