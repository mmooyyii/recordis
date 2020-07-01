-module(meck_redis).
-author("yimo").

%% API
-include_lib("meck_redis.hrl").
-export([start/1]).

start(Map) ->
    set_query_flow(Map),
    Fq = fun(_, Cmd) -> #redis_io{function = q, in = Cmd, out = O} = get_flow(), O end,
    Fqp = fun(_, Cmd) -> #redis_io{function = qp, in = Cmd, out = O} = get_flow(), O end,
    meck:expect(eredis, q, Fq),
    meck:expect(eredis, qp, Fqp).

set_query_flow(Map) ->
    put(step, 0),
    maps:fold(fun(K, V, _) -> put(K, V) end, '_', Map).

get_flow() ->
    Step = get(step),
    put(step, Step + 1),
    get(Step).

