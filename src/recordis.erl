-module(recordis).
-author("yimo").

%% API
-export([use/1, use/2]).
-export([new/1, get/1, update/1, delete/1]).

use(Client) ->
    put(recordis, Client).

use({Module, Q}, {Module, QP}) ->
    put(recordis, {{Module, Q}, {Module, QP}}).

new(Record) ->
    recordis_ctrl:new(Record).

get(Record) ->
    recordis_ctrl:get(Record).

update(Record) ->
    recordis_ctrl:update(Record).

delete(Record) ->
    recordis_ctrl:delete(Record).

