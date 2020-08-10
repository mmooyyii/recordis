-module(example).
-author("yimo").

%% API
-export([example/0]).
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
    id, name, code, a, b, c
}).

test_callback() ->
    #recordis_callback{
        before_new = [fun(#test{} = T) -> T#test{a = #{1 => 123}} end]
    }.

example() ->
    use(),
    eredis:q(test, [<<"FLUSHDB">>]),
    R = #test{
        id = <<"10001">>,
        name = <<"123">>,
        a = #{1 => 2},
        code = <<"ABC">>,
        c = [{1, 2}]
    },
    recordis:new(R),
    recordis:one(#test{id = <<"1">>}),
    recordis_find:all(#test{}, #recordis_where{column = code, condition = <<"ABC">>}).


use() ->
    case whereis(test) of
        undefined ->
            {ok, C} = eredis:start_link("127.0.0.1", 6379, 1),
            register(test, C);
        _ -> ok
    end,
    recordis:use(test).
