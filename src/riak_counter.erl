-module(riak_counter).
-behaviour(application).
-behaviour(gen_server).

-export([update/1,
         read/1,
         reset/1,
         delete/1]).

%% application callbacks
-export([start/2,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          pool = [] :: [pid()],
          all :: [pid()]
         }).

-define(BUCKET, <<"counters">>).
-type riak_obj() :: riakc_obj:riakc_obj().

%% API functions ---------------------------------------------------------------

-spec update(atom()) -> ok | {error, term()}.
update(CounterName) ->
    gen_server:call(?MODULE, {update, CounterName}).

-spec read(atom()) -> {ok, integer()} | {error, term()}.
read(CounterName) ->
    gen_server:call(?MODULE, {read, CounterName}).

-spec reset(atom()) -> ok | {error, term()}.
reset(CounterName) ->
    gen_server:call(?MODULE, {reset, CounterName}).

-spec delete(atom()) -> ok | {error, term()}.
delete(CounterName) ->
    gen_server:call(?MODULE, {delete, CounterName}).

%% application callbacks -------------------------------------------------------

start(_Type, _Args) ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    {ok, Pid, Pid}.

stop(Pid) ->
    exit(Pid, normal).

%% gen_server callbacks --------------------------------------------------------

init([]) ->
    {ok, Hosts} = application:get_env(riak_counter, hosts),
    {ok, Port} = application:get_env(riak_counter, port),
    Pool = [connect(Host, Port) || Host <- Hosts],
    {ok, #state{all = Pool}}.

handle_call({update, Name}, _From, State) ->
    {Pid, State2} = get_pid(State),
    Reply = update_in_riak(Name, Pid),
    {reply, Reply, State2};
handle_call({read, Name}, _From, State) ->
    {Pid, State2} = get_pid(State),
    Reply = read_from_riak(Name, Pid),
    {reply, Reply, State2};
handle_call({reset, Name}, _From, State) ->
    {Pid, State2} = get_pid(State),
    Reply = reset_in_riak(Name, Pid),
    {reply, Reply, State2};
handle_call({delete, Name}, _From, State) ->
    {Pid, State2} = get_pid(State),
    Reply = delete_from_riak(Name, Pid),
    {reply, Reply, State2};
handle_call(_Request, _From, State) ->
    {stop, bad_call, State}.

handle_cast(_Msg, State) ->
    {stop, bad_cast, State}.

handle_info(_Info, State) ->
    {stop, bad_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions ---------------------------------------------------------

-spec connect(string(), integer()) -> pid().
connect(Host, Port) ->
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),
    pong = riakc_pb_socket:ping(Pid),
    ok = riakc_pb_socket:set_bucket(Pid, ?BUCKET, [{allow_mult, true}]),
    Pid.

-spec get_pid(#state{}) -> {pid(), #state{}}.
get_pid(#state{pool = [], all = [Pid | Pool]} = State) ->
    {Pid, State#state{pool = Pool}};
get_pid(#state{pool = [Pid | Pool]} = State) ->
    {Pid, State#state{pool = Pool}}.

-spec update_in_riak(atom(), pid()) -> ok | {error, term()}.
update_in_riak(Name, Pid) ->
    NameBin = atom_to_binary(Name, utf8),
    Object = riakc_obj:new(?BUCKET, NameBin, <<"1">>),
    riakc_pb_socket:put(Pid, Object).

-spec read_from_riak(atom(), pid()) -> {ok, integer()} | {error, term()}.
read_from_riak(Name, Pid) ->
    NameBin = atom_to_binary(Name, utf8),
    case riakc_pb_socket:get(Pid, ?BUCKET, NameBin) of
        {ok, Object} ->
            Siblings = riakc_obj:get_values(Object),
            Value = lists:foldl(fun count_values/2, 0, Siblings),
            ValueBin = integer_to_binary(Value),
            Object2 = replace_siblings(ValueBin, Object),
            riakc_pb_socket:put(Pid, Object2),
            {ok, Value};
        {error, _Reason} = Error ->
            Error
    end.

-spec reset_in_riak(atom(), pid()) -> ok | {error, term()}.
reset_in_riak(Name, Pid) ->
    NameBin = atom_to_binary(Name, utf8),
    case riakc_pb_socket:get(Pid, ?BUCKET, NameBin) of
        {ok, Object} ->
            Object2 = replace_siblings(<<"0">>, Object),
            riakc_pb_socket:put(Pid, Object2);
        {error, notfound} ->
            Object = riakc_obj:new(?BUCKET, NameBin, <<"0">>),
            riakc_pb_socket:put(Pid, Object);
        {error, _Reason} = Error ->
            Error
    end.

-spec delete_from_riak(atom(), pid()) -> ok | {error, term()}.
delete_from_riak(Name, Pid) ->
    NameBin = atom_to_binary(Name, utf8),
    riakc_pb_socket:delete(Pid, ?BUCKET, NameBin).

-spec count_values(binary(), integer()) -> integer().
count_values(<<>>, Sum) ->
    Sum;
count_values(Value, Sum) ->
    Sum + binary_to_integer(Value).

-spec replace_siblings(binary(), riak_obj()) -> riak_obj().
replace_siblings(Replacement, Object) ->
    Object2 = riakc_obj:update_value(Object, Replacement),
    [{Metadata, _} | _] = riakc_obj:get_contents(Object2),
    riakc_obj:update_metadata(Object2, Metadata).
