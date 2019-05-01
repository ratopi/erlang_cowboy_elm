%%%-----------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 01. Mai 2019 17:30
%%%-------------------------------------------------------------------
-module(ece_ws).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([send_json/3, send_error/3]).

send_json(Result, Request, Opts) ->

	JSON = jsx:encode(Result),

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


send_error(Error, Request, Opts) ->

	Result =
		#{
			error => Error
		},

	JSON = jsx:encode(Result),

	Response =
		cowboy_req:reply(
			500,
			#{
				<<"content-type">> => <<"application/json">>
			},
			JSON,
			Request
		),

	{ok, Response, Opts}.
