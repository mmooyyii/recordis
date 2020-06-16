-module(recordis_hash).
-author("yimo").

%% API
-export([set/2]).

% read
set(Key, Map) ->
    case recordis_utils:flatten_map(Map) of
        [] -> [];
        Values -> [<<"HMSET">>, Key] ++ Values
    end.

% write


% private