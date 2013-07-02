.PHONY: rel all deps clean

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

test: 
	./rebar eunit

doc:
	./rebar doc

