-module(example).
-author("yimo").

%% API
-export([new/0]).


-record(test,
{
    column = [
        {id, primary_key},
        {name, string},
        {a, hash},
        {b, set},
        {c, sorted_set}
    ],
    link = [], callback = [],
    id, name, a, b, c
}).

new() ->
    case whereis(test) of
        undefined ->
            {ok, C} = eredis:start_link("127.0.0.1", 6379, 1),
            register(test, C);
        _ -> ok
    end,
    recordis:use(test),
    eredis:q(test, [<<"FLUSHDB">>]),
    Obj = #test{
        id = <<"10001">>,
        name = <<"123">>,
        a = #{1 => 2},
        c = [{1, 2}]
    },
    recordis:new(Obj).
