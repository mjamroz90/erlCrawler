%% @doc Modul wspomagajacy.
-module(session_manager).
-behaviour(gen_server).

-export([get_validity_time/1, set_validity_time/2, set_default_validity_time/1]).
-export([insert/4, re_insert/4, update/4, remove/1]).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

-record(state, {domain_validity_time_proplist, default_domain_validity_time}).

%% @type url() = string()

%% @spec start() -> {ok,Pid :: pid()} | {error,term()}
%% @doc Uruchamia proces serwera.
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec insert(Url :: url(), MaxDepth :: integer(), MaxBradth :: integer(), ValidityTime :: integer()) -> node()
%% @doc Tutaj nalezy przekazywac adresy poczatkowe wraz z parametrami. Odpytuje domain_managera o wezel i przekazuje adres do przetworzenia do scheduler'a na odpowiednim wezle.
insert(Url, MaxDepth, MaxWidth, ValidityTime) ->
	Domain = reg:get_domain(Url),
	case application:get_env(session_manager,domain_manager_node) of
		{ok, Node} ->
			DestinationNode = rpc:call(Node, domain_dispatch_server, insert, [Domain]);
		undefined ->
			DestinationNode = domain_dispatch_server:insert(Domain)
	end,
	Params = common:stick_params({timestamp, 0},
				common:stick_params({width, MaxWidth},
					common:stick_params({depth, MaxDepth}, [])
				)
			),
	rpc:call(DestinationNode, session_manager, set_validity_time, [Domain, ValidityTime]),
	rpc:call(DestinationNode, scheduler, insert, [Url, Params]),
	
	%spawn_link(session_manager, re_insert, [ValidityTime, DestinationNode, Url, Params]),
	timer:apply_interval(ValidityTime/1000, rpc, call, [DestinationNode, scheduler, insert, [Url, Params]]),
	DestinationNode.

%% @deprecated	
re_insert(ValidityTime, DestinationNode, Url, Params) ->
	timer:sleep(ValidityTime/1000),
	rpc:call(DestinationNode, scheduler, insert, [Url, Params]),
	re_insert(ValidityTime, DestinationNode, Url, Params).

	
%% @spec get_validity_time(Domain :: url()) -> int()
%% @doc Zwraca czas po ktorym nalezy ponownie odwiedzic strony z danej domeny (w mikrosekundach).
get_validity_time(Domain) ->
	gen_server:call(?MODULE,{get_domain_validity_time,Domain}).
	
%% @spec set_validity_time(Domain :: url(), ValidityTime :: int()) -> ok
%% @doc Ustawia czas po ktorym nalezy ponownie odwiedzic strony z danej domeny (w mikrosekundach).
set_validity_time(Domain, ValidityTime) ->
	gen_server:call(?MODULE,{set_domain_validity_time, Domain, ValidityTime}).
	
%% @spec set_default_validity_time(ValidityTime :: int()) -> ok
%% @doc Ustawia domyslny czas po ktorym nalezy ponownie odwiedzic strony z domen nie wyspecyfikowanych jako startowe (w mikrosekundach).
set_default_validity_time(ValidityTime) ->
	gen_server:call(?MODULE, {set_default_domain_validity_time, ValidityTime}).
	
%% @private
update(_Url, _MaxDepth, _MaxWidth, _ValidityTime) ->
	ok.

%% @private
remove(_Url) ->
	ok.

%tutaj chyba bedzie tez glowne api do statystyk

%==============================CallBacks================================


%% @private
init([]) ->
    {ok, #state{domain_validity_time_proplist = [], default_domain_validity_time = 1000000*3600*24}}.

%% @private
handle_call({set_domain_validity_time, Domain, ValidityTime}, _From, State = #state{domain_validity_time_proplist = DomainValidityTimeProplist}) ->
	NewState = State#state{domain_validity_time_proplist = common:stick_params({Domain, ValidityTime}, DomainValidityTimeProplist)},
    {reply,ok,NewState};
%% @private
handle_call({get_domain_validity_time, Domain}, _From, State = #state{domain_validity_time_proplist = DomainValidityTimeProplist, default_domain_validity_time = DefaultValidityTime}) ->
	Reply = case common:get_param(Domain, DomainValidityTimeProplist) of
		not_found -> DefaultValidityTime;
		Time -> Time
	end,
	{reply,Reply, State};
	
%% @private
handle_call({set_default_domain_validity_time, ValidityTime}, _From, State) ->
	{reply, ok, State#state{default_domain_validity_time = ValidityTime}}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
