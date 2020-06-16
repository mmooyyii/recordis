-module(recordis).
-author("yimo").

%% API
-export([use/1, use/2]).


use(Client) ->
    put(recordis, Client).


use({Module, Q}, {Module, QP}) ->
    put(recordis, {{Module, Q}, {Module, QP}}).
