-module(events_server).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(TIMER_GET, 1000).
-define(TIMER_STOP, 10000).
-define(MAX_EVENTS_CACHE, 30).
-define(D(X), io:format("DEBUG ~p@~p: ~p~n", [?MODULE, ?LINE, X])).

%% API
-export([
	start_link/0
	, get_events_from_id/2
	, start_get_events/0
	, stop_get_events/0
	, add_user_online/0
	, remove_user_online/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-record(state, {
	pid
	, url = "https://api.github.com/events"
	, running = false
	, users_online = 0
	, timer_get = null
	, timer_stop = null
	, events = []
	, last_id = 0
	, rate_limit = 0
	}).
-record(event, {id, type, json}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_get_events() ->
	gen_server:cast(?SERVER, start_get_events).

stop_get_events() ->
	gen_server:cast(?SERVER, stop_get_events).

add_user_online() ->
	gen_server:cast(?SERVER, add_user_online).

remove_user_online() ->
	gen_server:cast(?SERVER, remove_user_online).

get_events_from_id(LastId, LastRateLimit) ->
	gen_server:call(?SERVER, {get_events, to_integer(LastId), to_integer(LastRateLimit)}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
	{ok, #state{pid = self()}}.

handle_call({get_events, LastId, LastRateLimit}, _From, State) ->
	LastRateEvent = if
		LastRateLimit /= State#state.rate_limit ->
			[
				create_rate_limit_event(State#state.rate_limit)
				, create_users_online_event(State#state.users_online)
			];
		true ->
			[]
	end,
	GithubEvents = [Event#event.json || Event <- State#state.events, Event#event.id > LastId],
	GithubEventsLen = length(GithubEvents),
	Events = if
		GithubEventsLen > 4, LastId == 0, LastRateLimit == 0 ->
			lists:append(LastRateEvent, lists:nthtail(GithubEventsLen - 4, GithubEvents));
		GithubEventsLen > 0 ->
			lists:append(LastRateEvent, GithubEvents);
		true ->
			[]
	end,
	{reply, Events, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(start_get_events, #state{pid = Pid, running = false, timer_stop = TimerStop} = State) ->
	?D(start_get_events),
	stop_timer(TimerStop),
	TimerGet = erlang:send_after(?TIMER_GET, Pid, get_events),
	State2 = State#state{running = true, timer_get = TimerGet},
	{noreply, State2};
handle_cast(stop_get_events, #state{running = true, timer_get = TimerGet} = State) ->
	?D(stop_get_events),
	stop_timer(TimerGet),
	State2 = State#state{running = false, timer_get = null,
		events = [], last_id = 0, rate_limit = 0},
	{noreply, State2};
handle_cast(add_user_online, #state{timer_stop = TimerStop, users_online = UsersOnline} = State) ->
	stop_timer(TimerStop),
	State2 = State#state{users_online = UsersOnline + 1},
	?D([add_user_online, {users_online, State2#state.users_online}]),
	{noreply, State2};
handle_cast(remove_user_online, #state{users_online = UsersOnline, timer_stop = TimerStop} = State) ->
	State2 = State#state{users_online = UsersOnline - 1},
	?D([remove_user_online, {users_online, State2#state.users_online}]),
	State3 = case State2#state.users_online of
		0 ->
			stop_timer(TimerStop),
			TimerStop2 = erlang:send_after(?TIMER_STOP, State#state.pid, stop_get_events),
			State2#state{timer_stop = TimerStop2};
		_ ->
			State2
	end,
	{noreply, State3};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(get_events, #state{running = true} = State) ->
	State2 = case get_events(State#state.url, State#state.last_id) of
		{ok, {Events, LastId}, RateLimit} ->
			EventsFiltered = [
				case convert_markdown(Event) of
					{ok, Event2, NewRateLimit} ->
						State#state{rate_limit = NewRateLimit},
						Event2;
					{ok, Event2} -> Event2;
					{error, Reason} -> Event
				end || Event <- lists:filter(fun filter_event/1, Events)],
			EventsFilteredLen = length(EventsFiltered),
			if
				EventsFilteredLen > 0 ->
 					gproc:send({p, l, ws_events}, {events, lists:append(
 							[
 								create_rate_limit_event(RateLimit)
 								, create_users_online_event(State#state.users_online)
 							]
 							, [Event#event.json || Event <- EventsFiltered]
 						)});
 				true ->
 					ok
			end,
 			Events2 = State#state.events ++ EventsFiltered,
 			Events2Len = length(Events2),
 			Events3 = if
 				Events2Len > ?MAX_EVENTS_CACHE ->
 					lists:nthtail(Events2Len - ?MAX_EVENTS_CACHE, Events2);
 				true ->
 					Events2
 			end,
			State#state{
				events = Events3
				, last_id = LastId
				, rate_limit = RateLimit
			};
 		{error, Reason} ->
			?D({error, Reason}),
			State
	end,
	TimerGet = erlang:send_after(?TIMER_GET, State2#state.pid, get_events),
	State3 = State2#state{timer_get = TimerGet},
	{noreply, State3};
handle_info(stop_get_events, #state{running = true} = State) ->
	events_server:stop_get_events(),
	{noreply, State#state{timer_stop = null}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_events(Url, LastId) ->
	case httpc:request(Url) of
		{ok, {{_, Status, _}, Headers, Body}} ->
			RateLimit = list_to_integer(proplists:get_value("x-ratelimit-remaining", Headers)),
			case Status of
				200 -> {ok, process_events(Body, LastId), RateLimit};
				_ -> {ok, {[], LastId}, RateLimit}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

convert_markdown(#event{type = EventType, json = Json} = Event) when
		EventType == <<"CommitCommentEvent">>;
		EventType == <<"IssueCommentEvent">>;
		EventType == <<"PullRequestReviewCommentEvent">> ->
	{struct, Payload} = proplists:get_value(<<"payload">>, Json),
	{struct, Comment} = proplists:get_value(<<"comment">>, Payload),
	CommentBody = proplists:get_value(<<"body">>, Comment),
	{struct, Repo} = proplists:get_value(<<"repo">>, Json),
	Context = proplists:get_value(<<"name">>, Repo),
	Request = {"https://api.github.com/markdown"
		, []
		, "application/json"
		, iolist_to_binary(mochijson2:encode({struct, [
				{text, CommentBody}
				, {mode, <<"gfm">>}
				, {context, Context}
			]}))
	},
	case httpc:request(post, Request, [], []) of
		{ok, {{_, Status, _}, Headers, Body}} ->
			RateLimit = list_to_integer(proplists:get_value("x-ratelimit-remaining", Headers)),
			case Status of
				200 ->
					Comment2 = lists:map(fun
						({<<"body">>, _}) ->
							{<<"body">>, iolist_to_binary(Body)};
						(Tuple) -> Tuple
					end, Comment),
					Payload2 = lists:map(fun
						({<<"comment">>, _}) ->
							{<<"comment">>, {struct, Comment2}};
						(Tuple) -> Tuple
					end, Payload),
					Json2 = lists:map(fun
						({<<"payload">>, _}) ->
							{<<"payload">>, {struct, Payload2}};
						(Tuple) -> Tuple
					end, Json),
					{ok, Event#event{json = Json2}, RateLimit};
				_ -> {ok, Event, RateLimit}
			end;
		{error, Reason} ->
			{error, Reason}
	end;
convert_markdown(Event) -> {ok, Event}.

process_events(Data, LastId) ->
	Json = mochijson2:decode(Data),
	Events = [parse_event(X) || {struct, X} <- Json],
	Events2 = lists:sort(fun (A, B) -> A#event.id =< B#event.id end, Events),
	Events3 = [X || X <- Events2, X#event.id > LastId],
	LastId2 = lists:max([LastId | [X#event.id || X <- Events3] ]),
	{Events3, LastId2}.

parse_event(Event) ->
	#event{
		id = list_to_integer(binary_to_list(proplists:get_value(<<"id">>, Event)))
		, type = proplists:get_value(<<"type">>, Event)
		, json = Event
	}.

filter_event(#event{type = EventType} = _Event) when
		EventType == <<"FollowEvent">>;
		EventType == <<"ForkEvent">>;
		EventType == <<"CommitCommentEvent">>;
		EventType == <<"IssueCommentEvent">>;
		EventType == <<"PullRequestReviewCommentEvent">>;
		EventType == <<"WatchEvent">> ->
	true;
filter_event(_Event) ->
	false.

create_rate_limit_event(RateLimit) ->
	[
		{<<"type">>, <<"RateLimit">>}
		, {<<"value">>, RateLimit}
	].

create_users_online_event(UsersOnline) ->
	[
		{<<"type">>, <<"UsersOnline">>}
		, {<<"value">>, UsersOnline}
	].

to_integer(X) when is_binary(X) ->
	list_to_integer(binary_to_list(X));
to_integer(X) when is_list(X) ->
	list_to_integer(X);
to_integer(X) when is_integer(X) ->
	X.

stop_timer(null) -> ok;
stop_timer(Timer) when is_reference(Timer) ->
	erlang:cancel_timer(Timer).
