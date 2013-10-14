REBAR=$(shell which rebar || echo ./rebar)

.PHONY: rel all clean

all: compile

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit

doc:
	$(REBAR) doc

