REBAR3=$(shell which rebar3 || echo ./rebar3)

.PHONY: rel all clean

all: compile

compile:
	$(REBAR3) compile

clean:
	$(REBAR3) clean

test:
	$(REBAR3) eunit

doc:
	$(REBAR3) edoc

