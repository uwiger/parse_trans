.PHONY: rel all clean

all: compile

compile:
	./rebar compile

clean:
	./rebar clean

test: 
	./rebar eunit

doc:
	./rebar doc

