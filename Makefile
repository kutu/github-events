ifeq ( $(findstring, Windows, $(OS)), )
	rebar = escript rebar
else
	rebar = escript.exe rebar
endif
app = github_events
port = 5000

all:
	$(rebar) get-deps compile

run:
	$(rebar) compile
	erl -noshell \
		-env PORT $(port) \
		-pa ebin deps/*/ebin \
		-boot start_sasl \
		-sname $(app) \
		-s $(app)_app
