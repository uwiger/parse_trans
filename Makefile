REBAR=$(shell which rebar || echo ./rebar)

.PHONY: rel all deps clean

all: deps compile

compile:
	$(REBAR) compile

deps:
	./rebar get-deps

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit

doc:
	$(REBAR) doc

