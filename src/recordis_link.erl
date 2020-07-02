-module(recordis_link).
-author("yimo").

%% API
-export([is_linked/2, link/2, unlink/2, linked/2]).

is_linked(RecordA, RecordB) ->
    case is_linkable(RecordA, RecordB) of
        true ->
            {LinkKey, PkB} = link_args(RecordA, RecordB),
            recordis_redis:q(recordis_set:is_member(LinkKey, PkB));
        false ->
            false
    end.

link(RecordA, RecordB) ->
    case is_linkable(RecordA, RecordB) of
        true ->
            {LinkKey, PkB} = link_args(RecordA, RecordB),
            recordis_redis:q(recordis_set:set(LinkKey, PkB));
        false ->
            error
    end.

unlink(RecordA, RecordB) ->
    case is_linkable(RecordA, RecordB) of
        true ->
            {LinkKey, PkB} = link_args(RecordA, RecordB),
            recordis_redis:q(recordis_set:delete(LinkKey, PkB));
        false ->
            error
    end.

is_linkable(RecordA, RecordB) ->
    Links = recordis_utils:obj_link(RecordA),
    Type = recordis_utils:obj_type(RecordB),
    lists:member(Type, Links).

linked(RecordAWithPk, RecordBWithNothing) ->
    {LinkKey, _} = link_args(RecordAWithPk, RecordBWithNothing),
    recordis_redis:q(recordis_set:get(LinkKey)).

link_args(RecordA, RecordB) ->
    TypeA = recordis_utils:obj_type(RecordA),
    PkA = recordis_utils:obj_primary_key(RecordA),
    TypeB = recordis_utils:obj_type(RecordB),
    PkB = recordis_utils:obj_primary_key(RecordB),
    {recordis_utils:key_concat([TypeA, TypeB, PkA]), PkB}.