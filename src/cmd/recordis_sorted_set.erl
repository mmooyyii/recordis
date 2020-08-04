-module(recordis_sorted_set).
-author("yimo").

%% API
-include("recordis.hrl").

-export([get_all/1, get_score/3]).
-export([set/2]).
-export([remove/2]).

set(_Key, []) ->
    #redis_cmd{};
set(Key, Values) ->
    #redis_cmd{cmd = [<<"ZADD">>, Key] ++ recordis_type:redis(sorted_set, Values)}.

get_all(Key) ->
    #redis_cmd{
        cmd = [<<"ZRANGE">>, Key, 0, -1, <<"WITHSCORES">>],
        formatter = fun(Return) -> recordis_type:erl(sorted_set, Return) end
    }.

get_score(Key, Left, Right) ->
    #redis_cmd{
        cmd = [<<"ZRANGEBYSCORE">>, Key, Left, Right, <<"WITHSCORES">>],
        formatter = fun(Return) -> recordis_type:erl(sorted_set, Return) end
    }.

remove(Key, Elements) when is_list(Elements) -> #redis_cmd{cmd = [<<"ZREM">>, Key] ++ Elements};
remove(Key, Element) -> #redis_cmd{cmd = [<<"ZREM">>, Key, Element]}.