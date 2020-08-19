# recordis 

A Redis ORM for Erlang, 

## Overview

- ORM
- Extension Erlang Type: int,float,binary,term,map,proplist,sets
- Index for number and string
- Transaction(use SET NX)
- Callbacks(Before/After Create/Update/Delete)

## Quick Start
```erlang
-include("recordis.hrl").
-export([save/0,update/0,search/0,get/0,delete/0]).
%%% Create Model
-record(example,
{
    column = [{id, primary_key},{name, string},{age,int}],
    link = [],
    callback = example_callback(),
    index = [age],
    id, 
    name,
    age
}).
example_callback() ->
    #recordis_callback{before_new = [fun(#example{} = T) -> T#example{name = <<"123">>} end]}.
%%% Save to Redis
save()->
    {ok, C} = eredis:start_link("127.0.0.1", 6379, 1),
    recordis:use(C),
    R = #example{id = <<"1">>,name = <<"mmooyyii">>,age = 25},
    recordis:new(R).
%%% Update
update()->
    {ok, C} = eredis:start_link("127.0.0.1", 6379, 1),
    recordis:use(C),
    R = #example{id = <<"1">>,age = 26},
    recordis:update(R).
%%% Delete From Redis
delete()->
    {ok, C} = eredis:start_link("127.0.0.1", 6379, 1),
    recordis:use(C),
    R = #example{id = <<"1">>},
    recordis:delete(R).
%%% Get by Primary Key
get()->
    {ok, C} = eredis:start_link("127.0.0.1", 6379, 1),
    recordis:use(C),
    R = #example{id = <<"1">>},
    Ret1 = recordis:one(R),
    Ret2 = recordis:one(R,[name,age]),
    {Ret1,Ret2}. 
%%% Search On Index
search()->
    {ok, C} = eredis:start_link("127.0.0.1", 6379, 1),
    recordis:use(C),
    recordis_find:all(#example{}, #recordis_where{column = age, condition = {20,30}}).
```


### Create Model
```
-record(example,
{
    column = [{id, primary_key},{name, string},{age,int}],
    link = [],
    callback = example_callback(),
    index = [],
    id, 
    name,
    age
}).
```

#### column
declare columns name and type

|Recordis Type| Erlang Type| Describe |
|:----:|:----:|:----:|
| primary_key  | binary |     unique  |
| string  | binary |    |
| int  | integer |    |
| float  | float |    |
| term  | any |    |
| hash  | maps |    |
| set  | sets |    |
| sorted_set  | proplist |    |
| index_string  | binary | in 0-9 and A-Z and length less than 10 |

#### link
other record
#### callback  
recordis_callback in "recordis.hrl" 
#### index  
Index column must is in column and column type is int,float or index_string.
#### columns name  
Same name and same sort in column.

## Warning
Do not opearte 'recordis' in process dict