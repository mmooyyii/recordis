-module(recordis_type).
-author("yimo").

%% API
-export([redis/2, erl/2]).

redis(string, String) when is_binary(String) -> String;
redis(int, Int) when is_integer(Int) -> binary_to_integer(Int);
redis(float, Float) when is_float(Float) -> term_to_binary(Float);
redis(term, Term) -> term_to_binary(Term);

redis(hash, Map) -> maps:fold(fun(K, V, A) -> [K, V | A] end, [], Map);
redis(set, Set) -> sets:to_list(Set);
redis(sorted_set, PropList) -> lists:foldl(fun({K, V}, A) -> [K, V | A] end, [], PropList);
redis(_, V) when is_binary(V) -> V.

erl(string, String) when is_binary(String) -> String;
erl(int, Int) when is_binary(Int) -> binary_to_integer(Int);
erl(float, Float) when is_binary(Float) -> binary_to_float(Float);
erl(term, Term) -> binary_to_term(Term);

erl(hash, V) -> hash_to_map(V, #{});
erl(set, List) -> sets:from_list(List);
erl(sorted_set, V) -> sorted_set_to_proplist(V, []);
erl(_, V) -> V.

hash_to_map([K, V | Rest], Acc) ->
    hash_to_map(Rest, Acc#{K => V});
hash_to_map([], Acc) ->
    Acc.

sorted_set_to_proplist([V, Score | Rest], Acc) ->
    sorted_set_to_proplist(Rest, [{Score, V} | Acc]);
sorted_set_to_proplist([], Acc) ->
    lists:reverse(Acc).

