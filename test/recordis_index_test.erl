-module(recordis_index_test).
-author("yimo").

%% API
-export([random_index_string/0]).
-include_lib("eunit/include/eunit.hrl").


random_index_string() ->
    A = list_to_tuple("1234567890QWERTYUIOPASDFGHJKLZXCVBNM"),
    N = ceil(random:uniform() * size(A)),
    list_to_binary([element(N, A) || _ <- lists:seq(1, 10)]).

string_hash_collision_test() ->
    A = [random_index_string() || _ <- lists:seq(1, 10000)],
    B = lists:map(fun recordis_index:string_hash/1, A),
    true = (sets:size(sets:from_list(A)) =:= sets:size(sets:from_list(B))).

string_hash_compare_test() ->
    true = recordis_index:string_hash(<<"123">>) < recordis_index:string_hash(<<"1234">>).