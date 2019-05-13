%%%-------------------------------------------------------------------
%% @doc ece top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ece_app_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(_Args) ->
	SupFlags =
		#{
			strategy => one_for_one,
			intensity => 1,
			period => 5
		},

	ChildSpecs = [
		#{
			id => ece_global_counter,
			start => {ece_global_counter, start_link, []},
			restart => permanent,
			shutdown => brutal_kill,
			type => worker
		},
		#{
			id => ece_cowboy_sup,
			start => {ece_cowboy_sup, start_link, []},
			restart => permanent,
			shutdown => brutal_kill,
			type => supervisor
		}
	],

	{ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
