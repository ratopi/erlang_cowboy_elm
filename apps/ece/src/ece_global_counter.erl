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
-export([get/0, reset/0, increment/0, decrement/0, wait_for_change/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {counter = 0, pids = []}).

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

wait_for_change(Timeout) ->
	?SERVER ! {notify_me, self()},
	receive
		R = {counter_changed, _} ->
			R
	after
		Timeout ->
			no_change
	end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
	{ok, #state{}}.


handle_call(get, _From, State) ->
	{reply, {ok, State#state.counter}, State};

handle_call(reset, _From, State) ->
	notify_listeners(0, State#state.pids),
	{reply, ok, State#state{counter = 0}};

handle_call(increment, _From, State) ->
	NewCounter = State#state.counter + 1,
	notify_listeners(NewCounter, State#state.pids),
	{reply, {ok, NewCounter}, State#state{counter = NewCounter}};

handle_call(decrement, _From, State) ->
	NewCounter = State#state.counter - 1,
	notify_listeners(NewCounter, State#state.pids),
	{reply, {ok, NewCounter}, State#state{counter = NewCounter}};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({notify_me, Pid}, State) ->
	MonitorRef = erlang:monitor(process, Pid),
	{noreply, State#state{pids = [{MonitorRef, Pid} | State#state.pids]}};

handle_info(_M = {'DOWN', MonitorRef, _Type, _Object, _Info}, State) ->
	{noreply, State#state{pids = remove_monitor_ref(MonitorRef, State#state.pids)}};

handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

remove_monitor_ref(MonitorRef, List) ->
	remove_monitor_ref(MonitorRef, List, []).


remove_monitor_ref(_MonitorRef, [], List) ->
	List;

remove_monitor_ref(MonitorRef, [{MonitorRef, _Pid} | T], List) ->
	remove_monitor_ref(MonitorRef, T, List);

remove_monitor_ref(MonitorRef, [H | T], List) ->
	remove_monitor_ref(MonitorRef, T, [H | List]).



notify_listeners(NewCounter, []) ->
	ok;

notify_listeners(NewCounter, [{_, Pid} | T]) ->
	Pid ! {counter_changed, NewCounter},
	notify_listeners(NewCounter, T).
