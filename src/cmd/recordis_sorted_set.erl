-module(recordis_sorted_set).
-author("yimo").

%% API
-export([set/2]).

set(Key, Map) ->
    case recordis_utils:flatten_map(Map) of
        [] -> [];
        Values -> [<<"ZADD">>, Key] ++ Values
    end.
