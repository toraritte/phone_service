%%%-------------------------------------------------------------------
%% @doc content_storage_azure public API
%% @end
%%%-------------------------------------------------------------------

-module(content_storage_azure_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    content_storage_azure_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
