-module(riak_counter_proper).
-behaviour(proper_statem).

-export([prop_counter/0]).

%% proper_statem callbacks
-export([command/1,
         initial_state/0,
         precondition/2,
         next_state/3,
         postcondition/3]).

-include_lib("proper/include/proper.hrl").

-record(state, {
          value :: integer() | undefined
         }).

-define(COUNTER, test).
-define(commands(), proper_statem:parallel_commands(?MODULE)).
-define(run_commands(Commands),
        proper_statem:run_parallel_commands(?MODULE, Commands)).

%% Property -------------------------------------------------------------------

prop_counter() ->
    ok = application:start(riak_counter),
    ?FORALL(Commands, ?commands(),
            begin
                riak_counter:delete(?COUNTER),
                {_Sequence, _Parallel, Result} = ?run_commands(Commands),
                Result == ok
            end).

%% Statem ---------------------------------------------------------------------

command(_State) ->
    frequency([{40, {call, riak_counter, update, [?COUNTER]}},
               {40, {call, riak_counter, read, [?COUNTER]}},
               {10, {call, riak_counter, reset, [?COUNTER]}},
               {10, {call, riak_counter, delete, [?COUNTER]}}]).

initial_state() ->
    #state{value = undefined}.

precondition(_State, _Call) ->
    true.

next_state(State, ok, {call, riak_counter, update, _}) ->
    case State#state.value of
        undefined ->
            State#state{value = 1};
        Value ->
            State#state{value = Value + 1}
    end;
next_state(State, ok, {call, riak_counter, reset, _}) ->
    State#state{value = 0};
next_state(State, ok, {call, riak_counter, delete, _}) ->
    State#state{value = undefined};
next_state(State, _Res, _Call) ->
    State.

postcondition(State, {call, riak_counter, read, _}, Res) ->
    case Res of
        {ok, Value} ->
            State#state.value == Value;
        {error, notfound} ->
            State#state.value == undefined
    end;
postcondition(_State, _Call, _Res) ->
    true.
