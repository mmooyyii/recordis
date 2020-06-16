%%%-------------------------------------------------------------------
%% @doc recordis public API
%% @end
%%%-------------------------------------------------------------------

-module(recordis_app).

-behaviour(application).

-export([start/2, stop/1, start/0]).

start() ->
    application:start(recordis).

start(_StartType, _StartArgs) ->
    recordis_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
