%%%-------------------------------------------------------------------
%% @doc content_storage_local_filesystem public API
%% @end
%%%-------------------------------------------------------------------

-module(content_storage_local_filesystem_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    content_storage_local_filesystem_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
