-module(riak_counter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(COUNTER, test).

%% Fixtures -------------------------------------------------------------------

counter_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [fun delete_bug/0]}.

setup() ->
    {ok, Pid} = riak_counter:start_link(5),
    unlink(Pid),
    Pid.

teardown(Pid) ->
    riak_counter:delete(?COUNTER),
    exit(Pid, kill).

%% Tests ----------------------------------------------------------------------

delete_bug() ->
    ok = riak_counter:update(?COUNTER),
    ok = riak_counter:update(?COUNTER),
    ok = riak_counter:delete(?COUNTER),
    ok = riak_counter:update(?COUNTER),
    ?assertEqual({ok, 1}, catch riak_counter:read(?COUNTER)).
