-module(github_events_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(cowboy),
    application:start(gproc),
    application:start(inets),
    application:start(ssl),
    application:start(github_events).

start(_StartType, _StartArgs) ->
	io:format("starting app~n"),
	ssl:start(),

	Root = {directory, [<<"www">>]},
	Mimetypes = {mimetypes, [
		{<<".txt">>, [<<"text/plain">>]}
		, {<<".html">>, [<<"text/html">>]}
		, {<<".css">>, [<<"text/css">>]}
		, {<<".js">>, [<<"application/javascript">>]}
		, {<<".ico">>, [<<"image/x-icon">>]}
	]},
	Dispatch = [
		{'_', [
			{[<<"events">>], bullet_handler, [{handler, events_handler}]},
			{[], cowboy_http_static, [Root, {file, <<"index.html">>}, Mimetypes]},
			{['...'], cowboy_http_static, [Root, Mimetypes]}
		]}
	],
	Port = case os:getenv("PORT") of
		false -> 5000;
		Num -> list_to_integer(Num)
	end,
	io:format("port ~p~n", [Port]),
	cowboy:start_listener(my_http_listener, 10,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),

    github_events_sup:start_link().

stop(_State) ->
    ok.
