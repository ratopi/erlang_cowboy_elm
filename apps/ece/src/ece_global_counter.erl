%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 01. Mai 2019 15:37
%%%-------------------------------------------------------------------
-module(ece_global_counter).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get/0, reset/0, increment/0, decrement/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {counter = 0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get() ->
	gen_server:call(?SERVER, get).

reset() ->
	gen_server:call(?SERVER, reset).

increment() ->
	gen_server:call(?SERVER, increment).

decrement() ->
	gen_server:call(?SERVER, decrement).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	{ok, #state{}}.


handle_call(get, _From, State) ->
	{reply, {ok, State#state.counter}, State};

handle_call(reset, _From, State) ->
	{reply, ok, State#state{counter = 0}};

handle_call(increment, _From, State) ->
	NewCounter = State#state.counter + 1,
	{reply, {ok, NewCounter}, State#state{counter = NewCounter}};

handle_call(decrement, _From, State) ->
	NewCounter = State#state.counter - 1,
	{reply, {ok, NewCounter}, State#state{counter = NewCounter}};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Request, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
