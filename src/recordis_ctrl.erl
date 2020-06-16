-module(recordis_ctrl).
-author("yimo").

%% API
-export([
    new/1,
    delete/1,
    update/1,
    delete_relation/2,
    get/1,
    get/2,
    filter/1,
    order/0,
    range/0
]).

new(Record) ->
    recordis_model:new(Record).

%% update when primary key exist
update(_Record) -> ok.

delete_relation(_Self, _Other) -> ok.

delete(Record) ->
    %% 删除时会级联删除relation中的关系
    recordis_redis:delete(recordis_utils:all_keys(Record)).


get(RecordWithPk) ->
    get(RecordWithPk, []).
get(RecordWithPk, Keys) ->
    ok.


% #test{name = [eq,<<"123">>]}
%
%
%
%
%
%

filter(RecordWithKeys) ->
    ok.


%% filter函数，需要在该列有对应索引时才能生效，否则直接报错。
%% 有order_index
lt() -> ok.
lte() -> ok.
gt() -> ok.
gte() -> ok.
%% 有hash或order_index
eq() -> ok.
neq() -> ok.

%% 类型必须为list，set，sorted_set，hash,且建立了inverted_index
has() -> ok.

%% 只能用于sorted_set
order() -> ok.
%% 只能用于sorted_set
range() -> ok.