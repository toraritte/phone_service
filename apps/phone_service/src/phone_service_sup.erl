%%%-------------------------------------------------------------------
%% @doc phone_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(phone_service_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% API
%% ====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
      #{ strategy  => rest_for_one
       , intensity => 0
       , period    => 1
       }

    % TODO `content`  takes  its  time to  create  the  content
%          graph, and depending  on its size, it  may result is
%          startup  issues. If  this indeed  becomes an  issue,
%          "outsource" the  graph building  to a  process, that
%          sends a  message to  `ivr_sup` that  is ok  to start
%          accepting calls.
,   ChildSpecs =
        [ child_spec(user_db, worker)
        , child_spec(content, worker)
        , child_spec(ivr_sup, supervisor)
        ]
,   {ok, {SupFlags, ChildSpecs}}
.

%% ====================================================================
%% Internal functions
%% ====================================================================

child_spec(Id, Type) ->
    #{ id       => Id
     , start    => { Id, start_link, [] }
     , restart  => permanent
     , type     => Type
     }
.
