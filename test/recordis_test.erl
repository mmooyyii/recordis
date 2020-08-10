-module(recordis_test).
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
        {g, sorted_set},
        {i, index_string}
    ],
    link = [],
    callbacks = #recordis_callback{},
    index = [b, i],
    id, a, b, c, d, e, f, g, i
}).

save_type_test() ->
    meck_redis:start([
        #redis_io{function = q, in = [<<"SET">>, <<"lock:type:10000">>, 1, <<"EX">>, 30, <<"NX">>], out = {ok, <<"OK">>}},
        #redis_io{function = q, in = [<<"SISMEMBER">>, <<"type">>, <<"10000">>], out = {ok, <<"0">>}},
        #redis_io{function = qp, in =
        [
            [<<"SADD">>, <<"type">>, <<"10000">>],
            [<<"HMSET">>, <<"type:10000">>, i, <<"123132">>,
                d, erlang:term_to_binary(self()), c, float_to_binary(1.1),
                b, <<"2">>, a, <<"1">>],
            [<<"SADD">>, <<"type:e:10000">>, 1],
            [<<"HMSET">>, <<"type:f:10000">>, 1, 2],
            [<<"ZADD">>, <<"type:g:10000">>, 1, 2],
            [<<"ZADD">>, <<"i:type:b">>, 2, <<"10000">>],
            [<<"ZADD">>, <<"i:type:i">>, 73245720, <<"10000">>]
        ], out = [{ok, <<"1">>}, {ok, <<"OK">>}, {ok, <<"3">>}, {ok, <<"OK">>},
            {ok, <<"1">>}, {ok, <<"1">>}, {ok, <<"1">>}]},
        #redis_io{function = q, in = [<<"DEL">>, <<"lock:type:10000">>], out = {ok, <<"OK">>}}
    ]
    ),
    recordis:use(test),
    recordis:new(#type{
        id = <<"10000">>,
        a = <<"1">>,
        b = 2,
        c = 1.1,
        d = self(),
        e = sets:from_list([1]),
        f = #{1 => 2},
        g = [{1, 2}],
        i = <<"123132">>
    }).