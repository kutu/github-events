// turn off websocket for webkit
window.WebSocket = undefined;

// connect to server
var connected = false;
var lastId = 0, lastRateLimit = 0;
function connect() {
	var bulletUrl = "ws://" + document.location.host + "/events";
	var bullet = $.bullet(bulletUrl);
	bullet.onopen = function() {
		connected = true;
		addEvent({
			content: "Connected!"
			, type: "Log"
			, 'class': 'alert-success'
		});
	};
	bullet.onclose = function() {
		console.log("close");
	};
	bullet.onerror = function() {
		console.log("onerror");
	};
	bullet.ondisconnect = function() {
		if (connected) {
			connected = false;
			addEvent({
				content: "Disconnected."
				, type: "Log"
				, 'class': 'alert-error'
			});
		}
	};
	bullet.onmessage = function(event) {
		var events = JSON.parse(event.data);
		$.each(events, function(index, event) {
			addEvent(event);
			if ("id" in event) lastId = event.id;
			if (event.type == "RateLimit") lastRateLimit = event.value;
		});
		this.setURL(bulletUrl + "?last_id=" + lastId + "&last_rate_limit=" + lastRateLimit);
	};
}

// show event
function addEvent(json) {
	var events = $("#events");
	var item, tmpl;
	if (typeof json == "object" && "type" in json) {
		switch(json.type) {
			case "Log":
				item = $(templates.logTmpl.render(json));
				break;
			case "RateLimit":
				$("#rate-limit > span").text(json.value);
				var rateLimit = $("#rate-limit");
				if (rateLimit.css("visibility") == "hidden") {
					rateLimit.css({visibility: "visible", opacity: 0});
					rateLimit.animate({opacity: 1});
				}
				break;
			case "UsersOnline":
				$("#users-online > span").text(json.value);
				var usersOnline = $("#users-online");
				if (usersOnline.css("visibility") == "hidden") {
					usersOnline.css({visibility: "visible", opacity: 0});
					usersOnline.animate({opacity: 1});
				}
				break;
			case "CommitCommentEvent":
				tmpl = templates.commitCommentUrlTmpl;
			case "IssueCommentEvent":
				if (!tmpl) tmpl = templates.issueCommentUrlTmpl;
			case "PullRequestReviewCommentEvent":
				if (!tmpl) tmpl = templates.pullRequestCommentUrlTmpl;
				item = $(templates.commentTmpl.render(json, {url: tmpl}));
				break;
			case "FollowEvent":
				item = $(templates.followTmpl.render(json));
				break;
			case "ForkEvent":
				item = $(templates.forkTmpl.render(json));
				break;
			case "WatchEvent":
				item = $(templates.watchTmpl.render(json));
				break;
		}
	}

	if (!item) return;

	// offset "top" css-property for new events, and animate it
	events.stop();
	var hei = events.height();
	events.prepend(item);
	events.css("top", function(index, value) {
		var newHei = events.height();
		value = value == "auto" ? 0 : parseInt(value)
		return value - (newHei - hei);
	});
	events.animate({top: 0});

	// fadeOut and remove 5-th event
	events.find(".event").eq(5).fadeOut(1000, function() {
		$(this).remove();
		changeEventsHeight();
	});

	changeEventsHeight();

	// fadeIn for avatars
	var imgs = item.find("img");
	imgs.css({opacity: 0});
	imgs.load(function() {
		$(this).animate({opacity: 1});
	});
}

function changeEventsHeight() {
	$("#events-wrap").css({height: $("#events").height()});
}

// templates
var templates = {};

templates.logTmpl = Hogan.compile('\
	<div class="event alert {{ class }}">{{ content }}</div>');

templates.commentTmpl = Hogan.compile('\
	<div class="event well well-small"> \
		<div class="alert alert-success"> \
			<p><a href="{{> url }}" target="_blank">#{{ id }}</a> {{ type }} at {{ created_at }}</p> \
				<a href="https://github.com/{{ actor.login }}" target="_blank"> \
					<img src="{{ actor.avatar_url }}"> \
					<span class="label label-success">@{{ actor.login }}</span> \
				</a> \
				commented in \
				<a href="https://github.com/{{ repo.name }}" target="_blank"> \
					<span class="badge badge-success">{{ repo.name }}</span> \
				</a> \
		</div> \
		{{{ payload.comment.body }}} \
	</div>');

templates.commitCommentUrlTmpl = Hogan.compile('{{ payload.comment.html_url }}');
templates.issueCommentUrlTmpl = Hogan.compile('{{ payload.issue.html_url }}#issuecomment-{{ payload.comment.id }}');
templates.pullRequestCommentUrlTmpl = Hogan.compile('{{ payload.comment._links.html.href }}');

templates.followTmpl = Hogan.compile('\
	<div class="event alert"> \
		<p>#{{ id }} {{ type }} at {{ created_at }}</p> \
		<a href="https://github.com/{{ actor.login }}" target="_blank"> \
			<img src="{{ actor.avatar_url }}"> \
			<span class="label label-warning">@{{ actor.login }}</span> \
		</a> \
		followed \
		<a href="https://github.com/{{ payload.target.login }}" target="_blank"> \
			<img src="{{ payload.target.avatar_url }}"> \
			<span class="label label-warning">@{{ payload.target.login }}</span> \
		</a> \
	</div>');

templates.forkTmpl = Hogan.compile('\
	<div class="event alert alert-info"> \
		<p>#{{ id }} {{ type }} at {{ created_at }}</p> \
		<a href="https://github.com/{{ actor.login }}" target="_blank"> \
			<img src="{{ actor.avatar_url }}"> \
			<span class="label label-info">@{{ actor.login }}</span> \
		</a> \
		forked \
		<a href="{{ payload.forkee.html_url }}" target="_blank"> \
			<span class="badge badge-info">{{ payload.forkee.full_name }}</span> \
		</a> \
	</div>');

templates.watchTmpl = Hogan.compile('\
	<div class="event alert alert-info"> \
		<p>#{{ id }} {{ type }} at {{ created_at }}</p> \
		<a href="https://github.com/{{ actor.login }}" target="_blank"> \
			<img src="{{ actor.avatar_url }}"> \
			<span class="label label-info">@{{ actor.login }}</span> \
		</a> \
		{{ payload.action }} watch \
		<a href="https://github.com/{{ repo.name }}" target="_blank"> \
			<span class="badge badge-info">{{ repo.name }}</span> \
		</a> \
	</div>');

// dom ready
$(connect);
