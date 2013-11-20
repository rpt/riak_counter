.PHONY: all compile get-deps test clean

all: compile get-deps

compile:
	@ rebar compile

get-deps:
	@ rebar get-deps

test: compile
	@ rebar skip_deps=true eunit
	@ erl -pa .eunit deps/*/ebin -noshell \
	  -eval "proper:quickcheck(\
                 proper:numtests(100, \
                     riak_counter_proper:prop_counter())), \
	         init:stop()."

clean:
	@ rebar clean
	@ rmdir ebin
