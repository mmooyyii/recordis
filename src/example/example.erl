-module(example).
-author("yimo").

%% API
-export([new/0]).
-include("recordis.hrl").

-record(test,
{
    column = [
        {id, primary_key},
        {name, string},
        {code, index_string},
        {a, hash},
        {b, set},
        {c, sorted_set}
    ],
    link = [],
    callback = test_callback(),
    index = [code],
    id, name, a, b, c
}).

test_callback() ->
    #recordis_callback{
        before_new = [fun(#test{} = T) -> T#test{a = #{1 => 123}} end]
    }.

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
    recordis:new(Obj),
    recordis:one(Obj#test{id = <<"1">>}).