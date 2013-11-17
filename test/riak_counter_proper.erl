-module(riak_counter_proper).
-behaviour(proper_statem).

%% Properties
-export([prop_counter/0]).

%% proper_statem callbacks
-export([command/1,
         initial_state/0,
         precondition/2,
         next_state/3,
         postcondition/3]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
          value = 0 :: integer()
         }).

-define(MOD, riak_counter).
-define(COUNTER, test).
-define(assertProperty(P), ?assert(proper:quickcheck(P, [{to_file, user}]))).

%% EUnit wrapper --------------------------------------------------------------

counter_test_() ->
    {setup, fun setup/0, fun teardown/1,
     {timeout, 30, fun counter_property/0}}.

setup() ->
    {ok, Pid} = riak_counter:start(),
    Pid.

teardown(Pid) ->
    riak_counter:delete(?COUNTER),
    exit(Pid, kill).

counter_property() ->
    ?assertProperty(riak_counter_proper:prop_counter()).

%% Property -------------------------------------------------------------------

prop_counter() ->
    proper:numtests(
      1000,
      ?FORALL(Commands, proper_statem:commands(?MODULE),
              begin
                  riak_counter:reset(?COUNTER),
                  Output = proper_statem:run_commands(?MODULE, Commands),
                  {History, State, Result} = Output,
                  ?WHENFAIL(io:format("History: ~w\n"
                                      "State: ~w\n"
                                      "Result: ~w\n",
                                      [History, State, Result]),
                            aggregate(command_names(Commands),
                                      Result == ok))
              end)).

%% Statem ---------------------------------------------------------------------

command(_State) ->
    frequency([{50, {call, ?MOD, update, [?COUNTER]}},
               {40, {call, ?MOD, read, [?COUNTER]}},
               {10, {call, ?MOD, reset, [?COUNTER]}}]).

initial_state() ->
    #state{value = 0}.

precondition(_State, _Call) ->
    true.

next_state(State, ok, {call, ?MOD, update, _}) ->
    State#state{value = State#state.value + 1};
next_state(State, ok, {call, ?MOD, reset, _}) ->
    State#state{value = 0};
next_state(State, _Res, _Call) ->
    State.

postcondition(State, {call, ?MOD, read, _}, {ok, Value}) ->
    State#state.value == Value;
postcondition(_State, _Call, _Res) ->
    true.
