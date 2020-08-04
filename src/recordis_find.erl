-module(recordis_find).
-author("yimo").

%%%% API
-export([all/2]).
-include("recordis.hrl").


%% 因为查询规划器太难写了，而且redis多少有点不适合做这些，所以where中只能有一个列
%% 2次网络IO，不用lua也无可厚非吧。
all(Record, #recordis_where{column = Column, range = Expr}) when Column =/= undefined ->
    {Left, Right} = parse_range(Expr),
    case check_index(Record, Column) of
        true ->
            _Cmd = recordis_index:select(Record, Column, Left, Right),
            ok;
        _ ->
            throw(miss_index_error)
    end.

parse_range({A, B}) -> {A, B};
parse_range(Value) -> Value.

check_index(Record, Column) ->
    lists:member(Column, recordis_utils:index(Record)).