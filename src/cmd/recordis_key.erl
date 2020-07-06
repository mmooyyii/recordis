-module(recordis_key).
-author("yimo").

%% API

-include("recordis.hrl").

-export([delete/1, expire/2]).

delete(Keys) when is_list(Keys) ->
    lists:map(fun(Key) -> #redis_cmd{cmd = [<<"DEL">>, Key]} end, Keys);
delete(Key) ->
    #redis_cmd{cmd = [<<"DEL">>, Key]}.

expire(Key, TimeToLive) ->
    #redis_cmd{cmd = [<<"EXPIRE">>, Key, TimeToLive]}.