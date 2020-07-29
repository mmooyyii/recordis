-module(recordis).
-author("yimo").

-include("recordis.hrl").

-export([use/1, use/2]).
-export([new/1, delete/1, update/1]).
-export([one/1, all/1]).

use(Client) ->
    put(recordis, Client).

use({Module, Q}, {Module, QP}) ->
    put(recordis, {{Module, Q}, {Module, QP}}).

new(Record) ->
    recordis_ctrl:new(Record).

update(Record) ->
    recordis_ctrl:update(Record).

delete(Record) ->
    recordis_ctrl:delete(Record).

one(Record) ->
    recordis_ctrl:get(Record).

all(Record) ->
    all(Record, #recordis_where{}).

all(Record, Where) ->
    recordis_find:all(Record, Where).
