-module(recordis_key_test).
-author("yimo").

%% API
-include_lib("eunit/include/eunit.hrl").
-include("meck_redis.hrl").


delete_test() ->
    recordis:use(test),
    meck_redis:start([
        #redis_io{function = q, in = [<<"DEL">>, <<"abc">>], out = {ok, <<"0">>}},
        #redis_io{function = qp, in = [[<<"DEL">>, <<"abc">>]], out = [{ok, <<"1">>}]}
    ]),
    recordis_redis:q(recordis_key:delete(<<"abc">>)),
    recordis_redis:q(recordis_key:delete([<<"abc">>])).