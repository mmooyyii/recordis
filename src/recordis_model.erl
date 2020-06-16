-module(recordis_model).
-author("yimo").

%% API
-export([new/1]).

%% non data structure type key:
%% type: hash ,key {record}:{primary_key} -> {key: value}
%% data structure type key:
%% type: same as declare，key {record}:{key}:{primary_key} -> same as declare

%%  non data structure type key is called by n_key for short
%%  data structure type key is called by s_key for short
new(Record) ->
    case recordis_redis:exist(recordis_utils:primary_key(Record)) of
        false ->
            Pk = recordis_utils:obj_primary_key(Record),
            Type = recordis_utils:obj_type(Record),
            {NKeys, SKeys} = parse_record(Record),
            init_obj(Type, Pk, NKeys, SKeys);
        true ->
            throw(key_conflict_error)
    end.

init_obj(Type, Pk, NKeys, SKeys) ->
    PrimaryKey = recordis_utils:key_concat([Type, Pk]),
    NKeysCmd = recordis_hash:set(PrimaryKey, NKeys),
    SKeysCmd = s_key_to_cmd(Type, Pk, SKeys),
    Cmds = [NKeysCmd] ++ SKeysCmd,
    recordis_utils:yes_ok(recordis_redis:q(Cmds)).

parse_record(Record) ->
    Column = recordis_utils:obj_column(Record),
    {_, N, S} = lists:foldl(fun parse_column/2, {Record, #{}, #{}}, recordis_utils:enumerate(Column)),
    {N, S}.

parse_column({Index, Row}, {Record, N, S}) ->
    Value = recordis_utils:obj_value(Index, Record),
    case check_row(Row, Value) of
        {n, K, V} -> {Record, maps:put(K, V, N), S};
        {s, K, V} -> {Record, N, maps:put(K, V, S)};
        _ -> {Record, N, S}
    end.

%% TODO: check value type
check_row({_Key, _}, undefined) -> ignore;
check_row({_Key, primary_key}, _) -> ignore;
check_row({Key, KeyType}, Value) ->
    case recordis_utils:is_s_key(KeyType) of
        true -> {s, {atom_to_binary(Key, utf8), KeyType}, Value};
        false -> {n, atom_to_binary(Key, utf8), Value}
    end;
check_row(_, _) ->
    throw(type_error).

s_key_to_cmd(RecordType, Pk, SKey) ->
    maps:fold(fun({K, KeyType}, V, A) ->
        Key = recordis_utils:key_concat([RecordType, K, Pk]),
        case KeyType of
            hash -> [recordis_hash:set(Key, V) | A];
            set -> [recordis_set:set(Key, V) | A];
            sorted_set -> [recordis_sorted_set:set(Key, V) | A]
        end end, [], SKey).


