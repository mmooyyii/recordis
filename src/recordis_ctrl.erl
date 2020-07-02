-module(recordis_ctrl).
-author("yimo").

%% API
-export([
    new/1,
    delete/1,
    update/1,
    get/1,
    get/2,
    build_record/2
]).

%% non data structure type key:
%% type: hash ,key {record}:{primary_key} -> {key: value}
%% data structure type key:
%% type: same as declare，key {record}:{key}:{primary_key} -> same as declare

%%  non data structure type key is called by n_key for short
%%  data structure type key is called by s_key for short
new(Record) ->
    Type = recordis_utils:obj_type(Record),
    Pk = recordis_utils:obj_primary_key(Record),
    case recordis_redis:q(recordis_set:is_member(Type, Pk)) of
        false ->
            {NKeys, SKeys} = parse_record(Record),
            init_obj(Record,
                recordis_utils:without_value(NKeys, undefined),
                recordis_utils:without_value(SKeys, undefined)
            );
        true ->
            throw(key_conflict_error)
    end.

%% update when primary key exist
update(Record) ->
    Type = recordis_utils:obj_type(Record),
    Pk = recordis_utils:obj_primary_key(Record),
    case recordis_redis:q(recordis_set:is_member(Type, Pk)) of
        true ->
            {NKeys, SKeys} = parse_record(Record),
            init_obj(Record,
                recordis_utils:without_value(NKeys, undefined),
                recordis_utils:without_value(SKeys, undefined)
            );
        false ->
            throw(primary_key_not_find_error)
    end.

delete(Record) ->
    %% 删除时会级联删除relation中的关系
    Type = recordis_utils:obj_type(Record),
    Pk = recordis_utils:obj_primary_key(Record),
    recordis_redis:q(recordis_set:delete(Type, Pk)),
    recordis_redis:q(recordis_key:delete(recordis_utils:all_keys(Record))).

get(RecordWithPk) when is_tuple(RecordWithPk) ->
    erlang:hd(p_get([RecordWithPk], recordis_utils:obj_column(RecordWithPk))).
get(RecordWithPk, Keys) when is_tuple(RecordWithPk) ->
    erlang:hd(p_get([RecordWithPk], Keys)).

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

%%filter(RecordWithKeys) -> throw(todo_error).
%%%% filter函数，需要在该列有对应索引时才能生效，否则直接报错。
%%%% 有order_index
%%lt() -> ok.
%%lte() -> ok.
%%gt() -> ok.
%%gte() -> ok.
%%%% 有hash或order_index
%%eq() -> ok.
%%neq() -> ok.
%%%% 类型必须为list，set，sorted_set，hash,且建立了inverted_index
%%has() -> ok.
%%%% 只能用于sorted_set
%%order() -> ok.
%%%% 只能用于sorted_set
%%range() -> ok.

n_keys_to_map(NKeys) ->
    maps:fold(fun({Key, Type}, V, Acc) -> Acc#{Key => recordis_type:redis(Type, V)} end, #{}, NKeys).

build_record(Record, Map) ->
    Column = maps:from_list(recordis_utils:enumerate_r(recordis_utils:obj_column_key(Record))),
    F = fun(Key, Val, Acc) ->
        Index = maps:get(Key, Column),
        recordis_utils:obj_set_value(Index, Val, Acc)
        end,
    maps:fold(F, Record, Map).
