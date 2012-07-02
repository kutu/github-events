-module(events_handler).
-export([init/4, stream/3, info/3, terminate/2]).

init(_Transport, Req, _Opts, Active) ->
	events_server:start_get_events(),
	events_server:add_user_online(),
	{LastId, _} = cowboy_http_req:qs_val(<<"last_id">>, Req, 0),
	{LastRateLimit, _} = cowboy_http_req:qs_val(<<"last_rate_limit">>, Req, 0),
	IsGproc = case Active of
		true ->
			self() ! {events, events_server:get_events_from_id(LastId, LastRateLimit)},
			gproc:reg({p, l, ws_events}),
			true;
		once ->
			Events = events_server:get_events_from_id(LastId, LastRateLimit),
			case Events of
				[] ->
					gproc:reg({p, l, ws_events}),
					true;
				_ ->
					self() ! {events, Events},
					false
			end
	end,
	{ok, Req, IsGproc}.

stream(Data, Req, State) ->
	{reply, Data, Req, State}.

info({events, Events}, Req, State) ->
	{reply, mochijson2:encode(Events), Req, State};
info(_Info, Req, State) ->
	{ok, Req, State}.

terminate(_Req, IsGproc) ->
	case IsGproc of
		true ->	gproc:unreg({p, l, ws_events});
		_ -> ok
	end,
	events_server:remove_user_online().
