-module(recordis_link).
-author("yimo").

%% API
-export([
    is_linked/2,
    link/2,
    unlink/2]).

is_linked(RecordA, RecordB) ->
    case is_linkable(RecordA, RecordB) of
        true ->
            TypeA = recordis_utils:obj_type(RecordA),
            PkA = recordis_utils:obj_primary_key(RecordA),
            TypeB = recordis_utils:obj_type(RecordB),
            PkB = recordis_utils:obj_primary_key(RecordB),
            recordis_set:is_member(link_key(TypeA, TypeB, PkA), PkB);
        false ->
            error
    end.

link(_A, _B) -> ok.

unlink(_A, _B) -> ok.

link_key(A, B, PkA) ->
    recordis_utils:key_concat([A, B, PkA]).

is_linkable(RecordA, RecordB) ->
    Links = recordis_utils:obj_link(RecordA),
    Type = recordis_utils:obj_type(RecordB),
    lists:member(Type, Links).
