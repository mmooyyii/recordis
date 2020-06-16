-module(recordis_set).
-author("yimo").

%% API
-export([is_member/2, set/2]).

%% read
is_member(Key, Element) ->
    [<<"SISMEMBER">>, Key, Element].


% write
set(Key, Sets) ->
    case sets:to_list(Sets) of
        [] -> [];
        Values -> [<<"SADD">>, Key] ++ Values
    end.

% private

