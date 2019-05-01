%%%-------------------------------------------------------------------
%% @doc ece public API
%% @end
%%%-------------------------------------------------------------------

-module(ece_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	ece_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
	ok.

%%====================================================================
%% Internal functions
%%====================================================================