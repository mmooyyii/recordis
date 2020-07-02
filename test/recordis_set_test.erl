-module(recordis_set_test).
-author("yimo").

%% API
-include_lib("eunit/include/eunit.hrl").
-include("meck_redis.hrl").

is_member_test() ->
    meck_redis:start([
        #redis_io{function = q, in = [<<"SISMEMBER">>, <<"abc">>, <<"edf">>], out = {ok, <<"1">>}},
        #redis_io{function = q, in = [<<"SISMEMBER">>, <<"abc">>, <<"edf">>], out = {ok, <<"0">>}}
    ]),
    recordis:use(test),
    true = recordis_redis:q(recordis_set:is_member(<<"abc">>, <<"edf">>)),
    false = recordis_redis:q(recordis_set:is_member(<<"abc">>, <<"edf">>)).
