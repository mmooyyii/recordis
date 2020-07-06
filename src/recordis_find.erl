-module(recordis_find).
-author("yimo").

%% API
-export([]).

%%find(RecordWithKeys) -> throw(todo_error).
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