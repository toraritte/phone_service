%%%-------------------------------------------------------------------
%% @doc publication_guide public API
%% @end
%%%-------------------------------------------------------------------

-module(publication_guide_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    publication_guide_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
