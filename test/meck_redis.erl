-module(meck_redis).
-author("yimo").

%% API
-include_lib("meck_redis.hrl").
-export([start/1]).

start(Ls) ->
    set_query_flow(Ls),
    Fq = fun(_, Cmd) ->
        case get_flow() of
            #redis_io{function = q, in = Cmd, out = O} -> O;
            R -> throw({input_error, Cmd, R#redis_io.in})
        end end,
    Fqp = fun(_, Cmd) ->
        case get_flow() of
            #redis_io{function = qp, in = Cmd, out = O} -> O;
            R -> throw({input_error, Cmd, R#redis_io.in})
        end end,
    meck:expect(eredis, q, Fq),
    meck:expect(eredis, qp, Fqp).

set_query_flow(Ls) ->
    put(step, 1),
    put(query, Ls).

get_flow() ->
    Step = get(step),
    put(step, Step + 1),
    lists:nth(Step, get(query)).