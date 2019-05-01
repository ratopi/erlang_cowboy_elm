%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 13. Okt 2018 10:30
%%%-------------------------------------------------------------------
-module(ece_cowboy).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([start_link/0]).

start_link() ->
	{ok, _Pid} =
		cowboy:start_clear(
			api_http_listener,
			[
				{port, get_env(http_port, 8080)}
			],
			#{
				env => #{dispatch => get_cowboy_dispatch()}
			}
		).

%%====================================================================
%% Internal functions
%%====================================================================

get_cowboy_dispatch() ->
	Hosts = '_',
	Routes =
		[
			{"/", cowboy_static, {priv_file, ece, "static/index.html"}},
			% {"/ws", ece_websocket, []},
			{"/[...]", cowboy_static, {priv_dir, ece, "static"}}
		],
	Dispatch = cowboy_router:compile([{Hosts, Routes}]),
	Dispatch.


get_env(Key, Default) ->
	case application:get_env(Key) of
		{ok, Value} -> Value;
		_ -> Default
	end.
