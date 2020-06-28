-module(recordis_set).
-author("yimo").

%% API
-export([is_member/2, set/2, get/1]).
-include("recordis.hrl").


%% read
is_member(Key, Element) ->
    [<<"SISMEMBER">>, Key, Element].

% write
set(Key, Set) ->
    case sets:size(Set) of
        0 -> #redis_cmd{};
        _ -> #redis_cmd{cmd = [<<"SADD">>, Key] ++ recordis_type:redis(set, Set)}
    end.

get(Key) ->
    #redis_cmd{cmd = [<<"SMEMBERS">>, Key], transfer = {sets, from_list}}.




