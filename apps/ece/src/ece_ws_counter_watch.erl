%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 13. Mai 2019 21:19
%%%-------------------------------------------------------------------
-module(ece_ws_counter_watch).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-define(TIMEOUT, 60000).

%% API
-export([init/2]).


init(Request, Opts) ->

	LastCounter = get_counter_param(cowboy_req:parse_qs(Request)),

	{ok, CurrentCounter} = ece_global_counter:get(),

	case LastCounter of

		nil ->
			response(CurrentCounter, Request, Opts);

		CounterParam ->

			case CurrentCounter == CounterParam of

				false ->
					response(CurrentCounter, Request, Opts);

				true ->

					case ece_global_counter:wait_for_change(?TIMEOUT) of
						{counter_changed, Counter} ->
							response(Counter, Request, Opts);
						no_change ->
							response(no_change, Request, Opts)
					end

			end
	end.



response(no_change, Request, Opts) ->
	Result =
		#{
		},
	ece_ws:send_json(Result, Request, Opts);

response(Counter, Request, Opts) when is_integer(Counter) ->
	Result =
		#{
			counter => Counter
		},
	ece_ws:send_json(Result, Request, Opts).




get_counter_param([]) ->
	nil;

get_counter_param([{<<"counter">>, Value} | _]) ->
	try binary_to_integer(Value) of
		N -> N
	catch
		error:badarg -> nil
	end;

get_counter_param([_ | T]) ->
	get_counter_param(T).
