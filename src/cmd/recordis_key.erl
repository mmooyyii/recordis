-module(recordis_key).
-author("yimo").

%% API

-include("recordis.hrl").

-export([set_nx/2, set_nx/3]).
-export([delete/1, expire/2]).

delete(Keys) when is_list(Keys) ->
    lists:map(fun(Key) -> #redis_cmd{cmd = [<<"DEL">>, Key]} end, Keys);

delete(Key) ->
    #redis_cmd{cmd = [<<"DEL">>, Key]}.

expire(Key, TimeToLive) ->
    #redis_cmd{cmd = [<<"EXPIRE">>, Key, TimeToLive]}.

set_nx(Key, Value) ->
    #redis_cmd{cmd = [<<"SET">>, Key, Value, <<"NX">>], formatter = fun(R) -> R =:= <<"OK">> end}.
set_nx(Key, Value, Timeout) when Timeout > 0 andalso is_integer(Timeout) ->
    #redis_cmd{cmd = [<<"SET">>, Key, Value, <<"EX">>, Timeout, <<"NX">>], formatter = fun(R) -> R =:= <<"OK">> end}.


