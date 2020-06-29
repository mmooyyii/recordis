-module(recordis_sorted_set).
-author("yimo").

%% API
-export([set/2, get/1]).
-include("recordis.hrl").


set(_Key, []) ->
    #redis_cmd{};
set(Key, Values) ->
    #redis_cmd{cmd = [<<"ZADD">>, Key] ++ recordis_type:redis(sorted_set, Values)}.

get(Key) ->
    #redis_cmd{
        cmd = [<<"ZRANGE">>, Key, 0, -1, <<"WITHSCORES">>],
        transfer = fun(Return) -> recordis_type:erl(sorted_set, Return) end
    }.