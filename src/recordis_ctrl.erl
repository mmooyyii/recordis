-module(recordis_ctrl).
-author("yimo").


-export([new/1]).
-export([get/1, get/2]).
-export([update/1]).
-export([delete/1]).

%% non data structure type key:
%% type: hash ,key {record}:{primary_key} -> {key: value}
%% data structure type key:
%% type: same as declare，key {record}:{key}:{primary_key} -> same as declare
%%  non data structure type key is called by n_key for short
%%  data structure type key is called by s_key for short
-spec new(tuple()) -> no_return() | throw_error.
new(PreRecord) ->
    Record = recordis_callback:before_new(PreRecord),
    Type = recordis_utils:obj_type(Record),
    Pk = recordis_utils:obj_primary_key(Record),
    recordis_lock:acquire(Record),
    case recordis_redis:q(recordis_set:is_member(Type, Pk)) of
        false ->
            {NKeys, SKeys} = parse_record(Record),
            Normal = recordis_utils:without_value(NKeys, undefined),
            Special = recordis_utils:without_value(SKeys, undefined),
            init_obj(Record, Normal, Special),
            recordis_lock:release(Record);
        true ->
            recordis_lock:release(Record),
            throw(key_conflict_error)
    end.

%% update when primary key exist
-spec update(tuple()) -> no_return() | throw_error.
update(PreRecord) ->
    Record = recordis_callback:before_new(PreRecord),
    Type = recordis_utils:obj_type(Record),
    Pk = recordis_utils:obj_primary_key(Record),
    recordis_lock:acquire(Record),
    case recordis_redis:q(recordis_set:is_member(Type, Pk)) of
        true ->
            {NKeys, SKeys} = parse_record(Record),
            Normal = recordis_utils:without_value(NKeys, undefined),
            Special = recordis_utils:without_value(SKeys, undefined),
            init_obj(Record, Normal, Special),
            recordis_lock:release(Record);
        false ->
            recordis_lock:release(Record),
            throw(primary_key_not_find_error)
    end.

-spec delete(tuple()) -> no_return() | throw_error.
delete(PreRecord) ->
    %% 删除时会级联删除relation中的关系
    Record = recordis_callback:before_new(PreRecord),
    Type = recordis_utils:obj_type(Record),
    Pk = recordis_utils:obj_primary_key(Record),
    recordis_lock:acquire(Record),
    recordis_redis:q(recordis_set:delete(Type, Pk)),
    recordis_redis:q(recordis_key:delete(recordis_utils:all_keys(Record))),
    recordis_lock:release(Record).

-spec get(tuple()) -> tuple().
get(RecordWithPk) when is_tuple(RecordWithPk) ->
    recordis_lock:acquire(RecordWithPk),
    Rt = erlang:hd(p_get([RecordWithPk], recordis_utils:obj_column(RecordWithPk))),
    recordis_lock:release(RecordWithPk),
    Rt.

-spec get(tuple(), list()) -> tuple().
get(RecordWithPk, Keys) when is_tuple(RecordWithPk) ->
    recordis_lock:acquire(RecordWithPk),
    Rt = erlang:hd(p_get([RecordWithPk], Keys)),
    recordis_lock:release(RecordWithPk),
    Rt.

p_get(Records, Keys) when is_list(Records) ->
    Cmds = lists:map(fun(Record) -> get_cmd(Record, Keys) end, Records),
    CmdPipe = lists:concat(Cmds),
    Rts = recordis_utils:lists_div(length(Records), recordis_redis:q(CmdPipe)),
    lists:map(fun(Rt) -> format_get_return(erlang:hd(Records), Keys, Rt) end, Rts).

format_get_return(Record, Keys, [Main | Struct]) ->
    SKeyWithType = lists:filter(fun({_Key, Type}) -> recordis_utils:is_s_key(Type) end, Keys),
    SKeys = [K || {K, _T} <- SKeyWithType],
    build_record(Record, maps:merge(Main, maps:from_list(lists:zip(SKeys, Struct)))).


get_cmd(Record, Keys) ->
    {N, S} = parse_record(Record),
    NKeys = maps:keys(maps:with(Keys, N)),
    get_n_keys(Record, NKeys) ++ get_s_keys(Record, S).

get_n_keys(Record, NKeys) ->
    Pk = recordis_utils:primary_key(Record),
    [recordis_hash:get(Pk, NKeys)].

get_s_keys(Record, SKeys) ->
    lists:map(fun
                  ({hash, K, _}) -> recordis_hash:get(K);
                  ({set, K, _}) -> recordis_set:get(K);
                  ({sorted_set, K, _}) -> recordis_sorted_set:get(K)
              end, redis_s_key(Record, SKeys)).

init_obj(Record, NKeys, SKeys) ->
    Type = recordis_utils:obj_type(Record),
    Pk = recordis_utils:obj_primary_key(Record),
    PkCmd = recordis_set:set(Type, Pk),
    PrimaryKey = recordis_utils:primary_key(Record),
    NKeysCmd = recordis_hash:set(PrimaryKey, n_keys_to_map(NKeys)),
    S_Keys = redis_s_key(Record, SKeys),
    SaveCmd = save_redis(S_Keys),
    Cmds = [PkCmd, NKeysCmd] ++ lists:reverse(SaveCmd),
    recordis_redis:q(Cmds).

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
check_row({_Key, primary_key}, _) -> ignore;
check_row({Key, KeyType}, Value) ->
    case recordis_utils:is_s_key(KeyType) of
        true -> {s, {Key, KeyType}, Value};
        false -> {n, {Key, KeyType}, Value}
    end;
check_row(_, _) -> throw(type_error).

redis_s_key(Record, SKey) ->
    Pk = recordis_utils:obj_primary_key(Record),
    Type = recordis_utils:obj_type(Record),
    maps:fold(fun({K, KeyType}, V, A) ->
        [{KeyType, recordis_utils:key_concat([Type, K, Pk]), V} | A]
              end, [], SKey).

save_redis(SKeys) ->
    lists:map(
        fun
            ({hash, Key, Val}) -> recordis_hash:set(Key, Val);
            ({set, Key, Val}) -> recordis_set:set(Key, Val);
            ({sorted_set, Key, Val}) -> recordis_sorted_set:set(Key, Val)
        end,
        SKeys).

n_keys_to_map(NKeys) ->
    maps:fold(fun({Key, Type}, V, Acc) -> Acc#{Key => recordis_type:redis(Type, V)} end, #{}, NKeys).

build_record(Record, Map) ->
    Column = maps:from_list(recordis_utils:enumerate_r(recordis_utils:obj_column_key(Record))),
    F = fun(Key, Val, Acc) ->
        Index = maps:get(Key, Column),
        recordis_utils:obj_set_value(Index, Val, Acc)
        end,
    maps:fold(F, Record, Map).
