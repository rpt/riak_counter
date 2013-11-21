-module(riak_counter_tests).

-include_lib("eunit/include/eunit.hrl").

-define(COUNTER, test).

%% Fixtures -------------------------------------------------------------------

counter_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [{timeout, 30, fun parallel_reads/0},
      fun delete_bug/0]}.

setup() ->
    {ok, Pid} = riak_counter:start_link(5),
    unlink(Pid),
    Pid.

teardown(Pid) ->
    riak_counter:delete(?COUNTER),
    exit(Pid, kill).

%% Tests ----------------------------------------------------------------------

parallel_reads() ->
    N = 100,
    Pid = self(),
    ok = riak_counter:reset(?COUNTER),
    spawn(fun() ->
              [ok = riak_counter:update(?COUNTER) || _ <- lists:seq(1, N)],
              Pid ! updates_done
          end),
    ok = wait_for_updates(),
    ?assertEqual({ok, N}, riak_counter:read(?COUNTER)).

delete_bug() ->
    ok = riak_counter:update(?COUNTER),
    ok = riak_counter:update(?COUNTER),
    ok = riak_counter:delete(?COUNTER),
    ok = riak_counter:update(?COUNTER),
    ?assertEqual({ok, 1}, catch riak_counter:read(?COUNTER)).

%% Helper functions -----------------------------------------------------------

wait_for_updates() ->
    receive
        updates_done ->
            ok
    after
        1 ->
            {ok, _} = riak_counter:read(?COUNTER),
            wait_for_updates()
    end.
