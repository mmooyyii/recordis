-module(recordis_redis).
-author("yimo").

%% API
-export([q/1]).
-export([query/1, query_pipe/1]).
-include("recordis.hrl").

q(Cmd) when is_record(Cmd, redis_cmd) ->
    erlang:hd(format_return([Cmd], [query(Cmd)]));
q(C) when is_list(C) ->
    Cmds = remove_empty(C),
    format_return(Cmds, query_pipe(Cmds)).

query(#redis_cmd{cmd = Cmd}) ->
    case client_or_function() of
        {{Module, Q}, _} -> apply(Module, Q, [remove_empty(Cmd)]);
        Client -> eredis:q(Client, Cmd)
    end.
query_pipe(C) ->
    Cmds = lists:map(fun(#redis_cmd{cmd = Cmd}) -> Cmd end, C),
    case client_or_function() of
        {_, {Module, QP}} -> apply(Module, QP, [Cmds]);
        Client -> eredis:qp(Client, Cmds)
    end.
client_or_function() ->
    case get(recordis) of
        undefined -> throw(status_error);
        {{Module1, Q}, {Module2, QP}} -> {{Module1, Q}, {Module2, QP}};
        Client -> Client
    end.

remove_empty(Cmd) when is_list(Cmd) ->
    lists:filter(fun(#redis_cmd{cmd = undefined}) -> false;(_) -> true end, Cmd).

format_return(Cmds, Return) when length(Cmds) =:= length(Return) ->
    format_return(Cmds, Return, []).
format_return([#redis_cmd{transfer = {M, F}} | Cmds], [{ok, R} | RedisData], Acc) ->
    format_return(Cmds, RedisData, [apply(M, F, [R]) | Acc]);
format_return([#redis_cmd{transfer = undefined} | Cmds], [{ok, R} | RedisData], Acc) ->
    format_return(Cmds, RedisData, [R | Acc]);
format_return([#redis_cmd{transfer = T} | Cmds], [{ok, R} | RedisData], Acc) ->
    format_return(Cmds, RedisData, [T(R) | Acc]);
format_return([], [], Acc) ->
    lists:reverse(Acc).
