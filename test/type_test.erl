-module(type_test).
-author("yimo").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").
-include("meck_redis.hrl").
-include("recordis.hrl").
-record(type, {
    column = [
        {id, primary_key},
        {a, string},
        {b, int},
        {c, float},
        {d, term},
        {e, set},
        {f, hash},
        {g, sorted_set}
    ],
    link = [],
    callbacks = #recordis_callback{},
    index = [],
    id, a, b, c, d, e, f, g
}).

save_type_test() ->
    meck_redis:start([
        #redis_io{function = q, in = [<<"SISMEMBER">>, <<"type">>, <<"10000">>], out = {ok, <<"0">>}},
        #redis_io{function = qp, in =
        [
            [<<"SADD">>, <<"type">>, <<"10000">>],
            [<<"HMSET">>, <<"type:10000">>, d, erlang:term_to_binary(self()), c, float_to_binary(1.1),
                b, <<"2">>, a, <<"1">>],
            [<<"SADD">>, <<"type:e:10000">>, 1],
            [<<"HMSET">>, <<"type:f:10000">>, 1, 2],
            [<<"ZADD">>, <<"type:g:10000">>, 1, 2]
        ], out = [{ok, <<"1">>}, {ok, <<"OK">>}, {ok, <<"3">>}, {ok, <<"OK">>}, {ok, <<"1">>}]}
    ]
    ),
    recordis:use(test),
    recordis:new(#type{id = <<"10000">>, a = <<"1">>, b = 2,
        c = 1.1, d = self(), e = sets:from_list([1]), f = #{1 => 2}, g = [{1, 2}]
    }).

