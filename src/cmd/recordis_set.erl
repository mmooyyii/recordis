-module(recordis_set).
-author("yimo").

%% API
-export([is_member/2, set/2, get/1]).
-include("recordis.hrl").

-define(EmptySet, {set, 0, 16, 16, 8, 80, 48,
    {[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []},
    {{[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []}}}).

%% read
is_member(Key, Element) ->
    [<<"SISMEMBER">>, Key, Element].

% write
set(_Key, ?EmptySet) ->
    #redis_cmd{};
set(Key, Set) ->
    #redis_cmd{
        cmd = [<<"SADD">>, Key] ++ recordis_type:redis(set, Set)
    }.


get(Key) ->
    #redis_cmd{cmd = [<<"SMEMBERS">>, Key], transfer = {sets, from_list}}.




