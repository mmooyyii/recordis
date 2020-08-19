-module(recordis_link).
-author("yimo").

%% API
-export([is_linked/2, link/2, unlink/2, linked/2]).

is_linked(RecordA, RecordB) ->
    true = is_linkable(RecordA, RecordB),
    {LinkKey, PkB} = link_args(RecordA, RecordB),
    recordis_redis:q(recordis_set:is_member(LinkKey, PkB)).

link(RecordA, RecordB) ->
    true = is_linkable(RecordA, RecordB),
    {LinkKey, PkB} = link_args(RecordA, RecordB),
    recordis_redis:q(recordis_set:set(LinkKey, PkB)).

unlink(RecordA, RecordB) ->
    true = is_linkable(RecordA, RecordB),
    {LinkKey, PkB} = link_args(RecordA, RecordB),
    recordis_redis:q(recordis_set:delete(LinkKey, PkB)).

is_linkable(RecordA, RecordB) ->
    Links = recordis_utils:link(RecordA),
    Type = recordis_utils:type(RecordB),
    lists:member(Type, Links).

linked(RecordAWithPk, RecordBWithNothing) ->
    {LinkKey, _} = link_args(RecordAWithPk, RecordBWithNothing),
    recordis_redis:q(recordis_set:get(LinkKey)).

link_args(RecordA, RecordB) ->
    TypeA = recordis_utils:type(RecordA),
    PkA = recordis_utils:pk(RecordA),
    TypeB = recordis_utils:type(RecordB),
    PkB = recordis_utils:pk(RecordB),
    {recordis_utils:key_concat([TypeA, TypeB, PkA]), PkB}.