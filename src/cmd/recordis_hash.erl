-module(recordis_hash).
-author("yimo").

%% API
-export([set/2, get/1, get/2]).
-include("recordis.hrl").

set(_Key, Map) when Map =:= #{} ->
    #redis_cmd{};
set(Key, Map) ->
    #redis_cmd{cmd = [<<"HMSET">>, Key] ++ recordis_type:redis(hash, Map)}.


get(Key) ->
    #redis_cmd{cmd = [<<"HGETALL">>, Key], formatter = {recordis_utils, un_flatten_map}}.
get(_Key, []) ->
    #redis_cmd{};
get(Key, KeysWithType) ->
    Keys = lists:map(fun({K, _T}) -> K;(K) -> K end, KeysWithType),
    #redis_cmd{
        cmd = [<<"HMGET">>, Key] ++ Keys,
        formatter = fun(Return) -> make_map(KeysWithType, Return) end
    }.

% private
make_map(KeysWithType, Return) when length(KeysWithType) =:= length(Return) ->
    make_map(KeysWithType, Return, #{}).

make_map([{Key, Type} | Rest], [R | Return], Acc) ->
    V = recordis_type:erl(Type, R),
    make_map(Rest, Return, Acc#{Key => V});
make_map([], [], Acc) ->
    Acc.