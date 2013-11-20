.PHONY: all compile get-deps test clean

N := 100

all: compile get-deps

compile:
	@ rebar compile

get-deps:
	@ rebar get-deps

test: compile
	@ rebar skip_deps=true eunit
	@ erl -pa .eunit deps/*/ebin -noshell \
	  -eval "proper:quickcheck(\
                 proper:numtests($(N), \
                     riak_counter_proper:prop_counter())), \
	         init:stop()."

clean:
	@ rebar clean
	@ rmdir ebin
