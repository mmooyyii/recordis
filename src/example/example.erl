-module(example).
-author("yimo").

%% API
-export([new/0]).

-record(class, {
    column = [{id, primary_key}, {name, string}, {info, hash}],
    link = [student, teacher],
    id,
    name,
    info
}).

-record(student, {
    column = [{id, primary_key}, {name, string}, {age, string}],
    link = [class],
    id,
    name,
    ages
}).

-record(teacher, {
    column = [{id, primary_key}, {name, string}],
    link = [class],
    id,
    name
}).

-record(test,
{
    column = [
        {id, primary_key},
        {name, string},
        {a, hash},
        {b, set},
        {c, sorted_set}
    ],
    link = [],
    id,
    name,
    a,
    b,
    c
}).

new() ->
    case whereis(test) of
        undefined ->
            {ok, C} = eredis:start_link("127.0.0.1", 6379, 1),
            register(test, C);
        _ -> ok
    end,
    recordis:use(test),
    Obj = #test{
        id = <<"10001">>,
        name = <<"123">>,
        a = #{1 => 2},
        c = [{1, 2}]
    },
    recordis:delete(Obj),
    recordis:new(Obj),
    recordis:get(#test{id = <<"10001">>}),
    recordis:update(#test{id = <<"10001">>, a = #{2 => 2}}),
    recordis:get(#test{id = <<"10001">>}).

