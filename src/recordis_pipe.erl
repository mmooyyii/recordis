-module(recordis_pipe).
-author("yimo").

%% API
-export([enter_pipe/0, commit/0, is_pipe/0]).

%% 把原本应该运行的修改，新建，删除的cmd储存，在commit时通过pipeline统一提交
%% 对于查询接口正常返回
%% 在注重交互的情况下须谨慎使用。
enter_pipe() ->
    put(recordis_pipe, 1).

commit() ->
    case is_pipe() of
        true ->
            erlang:erase(recordis_pipe);
        false ->
            throw(status_error)
    end.
is_pipe() ->
    1 =:= get(recordis_pipe).
