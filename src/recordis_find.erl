-module(recordis_find).
-author("yimo").

%%%% API
-export([all/2]).
-include("recordis.hrl").


%% 因为查询规划器太难写了，而且redis多少有点不适合做这些，所以where中只能有一个列
%% 2次网络IO，不用lua也无可厚非吧。
all(Record, #recordis_where{keys = Keys, column = Column, condition = Condition}) ->
    {Left, Right} = condition(Condition),
    not check_index(Record, Column) andalso throw(miss_index_error),
    Cmd = recordis_index:select(Record, Column, Left, Right),
    Records = lists:map(fun({_, V}) -> recordis_utils:set_pk(V, Record) end, recordis_redis:q(Cmd)),
    case Keys of
        undefined -> recordis_ctrl:batch_get(Records);
        _ -> recordis_ctrl:batch_get(Records, Keys)
    end.

condition({A, B}) -> {A, B};
condition(Value) -> {Value, Value}.

check_index(Record, Column) ->
    lists:member(Column, recordis_utils:index(Record)).