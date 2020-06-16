-module(recordis_redis).
-author("yimo").

%% API
-export([q/1]).
-export([exist/1, delete/1]).

q(Cmd = [B | _]) when is_binary(B) ->
    case client_or_function() of
        {{Module, Q}, _} -> apply(Module, Q, [remove_empty(Cmd)]);
        Client -> eredis:q(Client, Cmd)
    end;
q(Cmd = [B | _]) when is_list(B) ->
    case client_or_function() of
        {_, {Module, QP}} -> apply(Module, QP, [remove_empty(Cmd)]);
        Client -> eredis:qp(Client, remove_empty(Cmd))
    end.

client_or_function() ->
    case get(recordis) of
        undefined -> throw(status_error);
        {{Module1, Q}, {Module2, QP}} -> {{Module1, Q}, {Module2, QP}};
        Client -> Client
    end.

remove_empty(Cmd) ->
    lists:filter(fun([]) -> false;(_) -> true end, Cmd).


exist(Key) ->
    case q([<<"EXISTS">>, Key]) of
        {ok, <<"1">>} -> true;
        {ok, <<"0">>} -> false
    end.

delete(Keys) when is_list(Keys) ->
    q(lists:map(fun(Key) -> [<<"DEL">>, Key] end, Keys));
delete(Key) ->
    q([<<"DEL">>, Key]).