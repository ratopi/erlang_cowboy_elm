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

init(Request, Method) ->
	case is_legal_method(Method) of
		true ->

			{ok, Counter} = apply(ece_global_counter, Method, []),

			Result =
				#{
					counter => Counter
				},

			ece_ws:send_json(Result, Request, Method);

		false ->
			ece_ws:send_error(<<"Illegal Method">>, Request, Method)
	end.


is_legal_method(get) -> true;
is_legal_method(increment) -> true;
is_legal_method(decrement) -> true;
is_legal_method(_) -> false.
