%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 01. Mai 2019 15:08
%%%-------------------------------------------------------------------
-module(ece_ws_hello).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([init/2]).

init(Request, Opts) ->

	Text = list_to_binary(
		io_lib:format("Request:~n~p~n~nState:~n~p~n", [Request, Opts])
	),

	Response =
		cowboy_req:reply(
			200,
			#{
				<<"content-type">> => <<"text/plain">>
			},
			Text,
			Request
		),

	{ok, Response, Opts}.
