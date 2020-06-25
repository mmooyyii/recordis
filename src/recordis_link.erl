-module(recordis_link).
-author("yimo").

%% API
-export([
    is_linked/2,
    link/2,
    unlink/2]).

is_linked(A, B) ->
    TypeA = recordis_utils:obj_type(A),
    PkA = recordis_utils:obj_primary_key(A),
    TypeB = recordis_utils:obj_type(B),
    PkB = recordis_utils:obj_primary_key(B),
    recordis_set:is_member(link_key(TypeA, TypeB, PkA), PkB),
    ok.

link(_A, _B) -> ok.

unlink(_A, _B) -> ok.

link_key(A, B, PkA) ->
    recordis_utils:key_concat([A, B, PkA]).