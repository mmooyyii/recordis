-module(recordis_set).
-author("yimo").

%% API
-export([is_member/2, set/2, get/1, delete/2]).
-include("recordis.hrl").


is_member(Key, Element) ->
    #redis_cmd{cmd = [<<"SISMEMBER">>, Key, Element], transfer = fun(V) -> V =:= <<"1">> end}.

delete(Key, Remove) ->
    #redis_cmd{cmd = [<<"SREM">>, Key, Remove]}.

set(Key, Set) when is_tuple(Set) ->
    case sets:size(Set) of
        0 -> #redis_cmd{};
        _ -> #redis_cmd{cmd = [<<"SADD">>, Key] ++ recordis_type:redis(set, Set)}
    end;
set(Key, Ele) ->
    #redis_cmd{cmd = [<<"SADD">>, Key, Ele]}.

get(Key) ->
    #redis_cmd{cmd = [<<"SMEMBERS">>, Key], transfer = {sets, from_list}}.
