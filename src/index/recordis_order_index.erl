-module(recordis_order_index).
-author("yimo").

%% API
-export([]).


%% obj的对应字段改变后，将string转为int，修改该索引内的score值
%% ZADD {type}:index int object_pk
