-module(recordis_concat).
-author("yimo").

-define(Delimiter, <<":">>).
%% API
-export([key_to_lock/1, soft_delete_key/1, to_hash_index/2]).


key_to_lock(Key) -> <<"lock", ?Delimiter/binary, Key/binary>>.

soft_delete_key(Key) ->
    <<Key/binary, ?Delimiter/binary, "d">>.

to_hash_index(Type, Column) ->
    <<"index", ?Delimiter/binary, Type/binary, ?Delimiter/binary, Column/binary>>.