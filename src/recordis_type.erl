-module(recordis_type).
-author("yimo").

%% API
-export([redis/2, erl/2]).



redis(string, V) when is_binary(V) -> V;
redis(hash, Map) -> maps:fold(fun(K, V, A) -> [K, V | A] end, [], Map);
redis(set, Set) -> sets:to_list(Set);
redis(sorted_set, PropList) -> lists:foldl(fun({K, V}, A) -> [K, V | A] end, [], PropList);
redis(_, V) when is_binary(V) -> V.

erl(string, V) when is_binary(V) -> V;
erl(int, V) when is_binary(V) -> binary_to_integer(V);
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

