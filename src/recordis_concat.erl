-module(recordis_concat).
-author("yimo").


-include("recordis.hrl").
%% API
-export([key_to_lock/1, soft_delete_key/1]).
-export([index/2]).

key_to_lock(Key) -> <<"lock", ?Delimiter/binary, Key/binary>>.

soft_delete_key(Key) -> <<Key/binary, ?Delimiter/binary, "d">>.

index(Record, Column) ->
    Col = atom_to_binary(Column,utf8),
    Type = recordis_utils:type(Record),
    <<"i", ?Delimiter/binary, Type/binary, ?Delimiter/binary, Col/binary>>.