%%%-------------------------------------------------------------------
%% @doc IVR supervisor for dynamic children per call
%% @end
%%%-------------------------------------------------------------------

-module(ivr_sup).

-behaviour(supervisor).

%% API
-export(
   [ start_link/0
   , start_worker/1
   ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% API
%% ====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [])
.

start_worker(Ref) ->
    {ok, Pid} =
        supervisor:start_child(?SERVER, [])
,   {Ref, Pid}
.

%% ====================================================================
%% Supervisor callbacks
%% ====================================================================

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->

    SupFlags =
      #{ strategy  => simple_one_for_one
       , intensity => 0
       , period    => 1
       }

,   ChildSpecs =
        % https://erlang.org/doc/design_principles/sup_princ.html#simple
        [ #{ id       => ignore
           , start    => { ivr, start_link, [] }
           , restart  => temporary
           , shutdown => brutal_kill
           , type     => worker
           }
        ]
,   {ok, {SupFlags, ChildSpecs}}
.

%% ====================================================================
%% Internal functions
%% ====================================================================
