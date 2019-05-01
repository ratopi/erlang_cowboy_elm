%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 01. Mai 2019 15:08
%%%-------------------------------------------------------------------
-module(ece_ws_counter).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([init/2]).

init(Request, Opts) ->

	{ok, Counter} = ece_global_counter:increment(),

	JSON = jsx:encode(
		#{
			counter => Counter
		}
	),

	Response =
		cowboy_req:reply(
			200,
			#{
				<<"content-type">> => <<"application/json">>
			},
			JSON,
			Request
		),

	{ok, Response, Opts}.
