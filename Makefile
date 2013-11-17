.PHONY: all compile get-deps test clean

all: compile get-deps

compile:
	@ rebar compile

get-deps:
	@ rebar get-deps

test: compile
	@ rebar skip_deps=true eunit

clean:
	@ rebar clean
	@ rmdir ebin
