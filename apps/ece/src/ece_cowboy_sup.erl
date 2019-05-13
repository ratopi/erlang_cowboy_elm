%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 01. Jan 2019 14:30
%%%-------------------------------------------------------------------
-module(ece_cowboy_sup).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([start_link/0]).

start_link() ->
	{ok, _Pid} =
		cowboy:start_clear(
			api_http_listener,
			[
				{port, application:get_env(ece, http_port, 8080)}
			],
			#{
				env => #{
					dispatch => get_cowboy_dispatch()
				}
			}
		).

%%====================================================================
%% Internal functions
%%====================================================================

get_cowboy_dispatch() ->
	Hosts = '_',
	Routes =
		[
			{"/hello", ece_ws_hello, whatever},

			{"/counter", ece_ws_counter, get},
			{"/counter/increment", ece_ws_counter, increment},
			{"/counter/decrement", ece_ws_counter, decrement},
			{"/counter/error", ece_ws_counter, error},

			{"/counter/watch", ece_ws_counter_watch, []},

			{"/never_come_back", ece_ws_never_come_back, 5000},

			% {"/ws", ece_websocket, []},

			{"/", cowboy_static, {priv_file, ece, "static/index.html"}},
			{"/[...]", cowboy_static, {priv_dir, ece, "static"}}
		],
	Dispatch = cowboy_router:compile([{Hosts, Routes}]),
	Dispatch.
