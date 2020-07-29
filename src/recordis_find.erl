-module(recordis_find).
-author("yimo").

%%%% API
-export([all/2]).
-include("recordis.hrl").


%% 因为查询规划器太难写了，而且redis多少有点不适合做这些，所以where中只能有一个列
%% 2次网络IO，不用lua也无可厚非吧。
all(Record, #recordis_where{column = Column, expr = Expr}) when Column =/= undefined ->
    {Type, Value} = parse_expr(Expr),
    case {Value, check_index(Type, Record, Column)} of
        {Value, true} ->
            Pk = recordis_hash_index:select(Record, Column, Value),
            recordis_ctrl:get(recordis_utils:set_pk(Pk, Record));
        {{_Left, _Right}, true} ->
            throw(todo_error);
%%            Pks = recordis_order_index:select(Record, Column, Left, Right),
%%            recordis_ctrl:get(recordis_utils:set_pk(Pks, Record));
        _ ->
            throw(search_error)
    end.

parse_expr({A, B}) -> {sorted, {A, B}};
parse_expr(Value) -> {hash, Value}.

check_index(Type, Record, Column) ->
    lists:member({Column, Type}, recordis_utils:index(Record)).