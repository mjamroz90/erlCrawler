%% @doc Modul zarzadzajacy przetwarzaniem adresow na wezle
%% @end
-module(scheduler).
-behaviour(gen_server).
-export([start_link/2,insert/2,completed/0, get_processed_count/0, get_uptime/0, get_mean_speed/0, stop/0]).
-record(state,{scheduler_start_time, process_limit, buffer_size, urls, current_process_count, processed_count, load_manager_counter, load_manager_timestamp}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
    
-define(URL_BUCKET,term_to_binary("Urls")).

%% @type proplist() = [{Key::term(), Value::term()}] 
    
%======================================API==================================
         
%% @spec start_link(ProcessLimit :: integer(), BufferSize :: integer()) -> {ok, Pid} | {error,term()}
%% @doc Uruchamia zarzadce z parametrami <dl>
%% <dt>ProcessLimit</dt><dd>maksymalna liczba rownoczesnie aktywnych procesow przetwarzajacych</dd>
%% <dt>BufferSize</dt><dd>rozmiar podrecznego bufora adresow do przetwarzania</dd>
%% </dl>
%% @end
start_link(ProcessLimit, BufferSize) ->
	gen_server:start_link({local,?MODULE},?MODULE,[ProcessLimit, BufferSize],[]).
	
%% @spec insert(Url :: string(), Params :: proplist()) -> ok
%% @doc Testuje adres i zwiazane z nim parametry na okolicznosc wczesniejszego wystepowania, aktualizuje dane o glebokosci/szerokosci i decyduje o przekazaniu do przetwarzania.
%% @end
insert(Url, Params) ->
	gen_server:call(?MODULE,{insert,{Url, Params}}).

%% @spec completed() -> ok
%% @doc Wywolanie wskazuje na zakonczenie jakiejs akcji (typowo zakonczenie przetwarzania adresu). Sprawdza mozliwosci uruchomienia przetwarzania kolejnego adresu i ewentualnie uruchamia przetwarzanie.
%% @end
completed() ->
	gen_server:cast(?MODULE,completed).

%% @spec get_processed_count() -> int()
%% @doc Zwraca liczbe przetworzonych adresow na wezle (od momentu uruchomienia schedulera)
%% @end
get_processed_count() ->
	gen_server:call(?MODULE, get_processed_count).
	
%% @spec get_uptime() -> int()
%% @doc Zwraca czas od uruchomienia schedulera (w mikrosekundach).
%% @end
get_uptime() -> 
	gen_server:call(?MODULE, get_uptime).
	
%% @spec get_mean_speed() -> float()
%% @doc Zwraca srednia liczbe stron przetworzonych na sekunde.
%% @end
get_mean_speed() ->
	1000000*get_processed_count()/get_uptime().

%% @spec stop() -> ok
%% @doc Zatrzymuje zarzadce.
%% @end
stop() ->
	gen_server:cast(?MODULE,stop).
	
%===================================Callbacks==============================

%% @private
init([ProcessLimit, BufferSize]) ->	
	State = #state{scheduler_start_time = common:timestamp(), process_limit = ProcessLimit, buffer_size = BufferSize, urls = [], current_process_count = 0, processed_count= 0, load_manager_counter = 0, load_manager_timestamp = common:timestamp()},
	reportLoad(State),%initial load
	{ok, State}.
	
%% @private		
handle_cast(stop,State) ->
	{stop,"Made to stop",State};

%% @private	
handle_cast(completed, State) ->
	CurrentBufferSize = length(State#state.urls),
	MaxBufferSize = State#state.buffer_size,
	
	case CurrentBufferSize < MaxBufferSize/4 of
		true ->
			NewUrls = pull_urls();
		false ->
			NewUrls = []
	end,
	
	Urls = State#state.urls,
	
	NewState = State#state{current_process_count = current_process_count(processing_sup:count_children()), urls = Urls ++ NewUrls},
	gen_server:cast(?MODULE, process_next),
	{noreply, NewState};

%% @private	
handle_cast(process_next, State) when State#state.process_limit /= State#state.current_process_count ->
	
	case State#state.urls of	
		[H | T] ->
			ProcessLimit = State#state.process_limit,
			
			case State#state.load_manager_counter of
				ProcessLimit ->
					reportLoad(State),
					LoadManagerCounter = 0,
					LoadManagerTimestamp = common:timestamp();
				Val ->
					LoadManagerCounter = Val+1,
					LoadManagerTimestamp = State#state.load_manager_timestamp
			end,	
	
			processing_sup:start_child(common:get_param(id, url_server:lookup(H)),H),
			
			ProcessedCount = State#state.processed_count,
			NewState = State#state{current_process_count = current_process_count(processing_sup:count_children()), urls = T, processed_count = ProcessedCount + 1, load_manager_counter = LoadManagerCounter, load_manager_timestamp = LoadManagerTimestamp},
			gen_server:cast(?MODULE, process_next);
			
		[] ->
			NewState = State#state{current_process_count = current_process_count(processing_sup:count_children())} 
	end,
		
	{noreply, NewState};

%% @private
handle_cast(process_next, State) ->
	{noreply, State}.

%% @private
handle_call(get_uptime, _From, State) ->
	Reply = common:timestamp() - State#state.scheduler_start_time,
	{reply, Reply, State};
	
%% @private
handle_call(get_processed_count, _From, State) ->
	Reply = State#state.processed_count,
	{reply, Reply, State};

%% @private	
handle_call({insert,{Url, Params}}, _From, State) ->	
	case url_server:lookup(Url) of
		not_found ->
			url_server:insert(Url, Params),
			Id = common:get_param(id, url_server:lookup(Url)),
			visited_urls_server:insert(Id);
		ExistingParams ->
			process_new_params(Url, ExistingParams, Params)
	end,
	{reply,ok,State}.
	
%% @private	
pull_urls() ->
	%Urls = disk_cache_server:pull_urls(100, false),
	%Urls = disk_cache_server:pull_urls(100),
	Urls = disk_cache_server:pull_urls(100), % due to a bug in disk_cache_server (gen_server call loop)
	%set_visited(Urls),
	Urls.
	
%set_visited([]) -> ok;
%set_visited([H|T]) ->
	%disk_cache_server:set_visited(H),
	%set_visited(T).

%% @private	
current_process_count(PropListOfCounts) ->
	[_, {active, Count}, _, _] = PropListOfCounts,
	Count.

%% @private	
process_new_params(Url, OldParams, NewParams) ->
	OldTimestamp = common:get_param(timestamp, OldParams),
	OldWidth = common:get_param(width, OldParams),
	OldDepth = common:get_param(depth, OldParams),
	
	Width = common:get_param(width, NewParams),
	Depth = common:get_param(depth, NewParams),
		
	if
		OldWidth > Width -> %bylismy blizej
			true;
		((Width > OldWidth) and (Depth >= 0)) or ((Width == OldWidth) and (Depth > OldDepth)) ->
			%jestesmy w mniejszej szerokosci badz w tej samej, ale na mniejszej glebokosci
			%Params = [{timestamp, OldTimestamp}, {width, Width}, {depth, Depth}],
			Params = common:stick_params({timestamp, OldTimestamp},
				common:stick_params({width, Width},
					common:stick_params({depth, Depth}, OldParams)
				)
			),
			url_server:update(Url, Params);
		true ->
			false
	end,

	%sprawdzamy timestamp czy czas juz odswiezyc
	ValidityTime = session_manager:get_validity_time(reg:get_domain(Url)),
	RefreshTime = OldTimestamp + ValidityTime,
	CurrentTime = common:timestamp(),
	if
		CurrentTime > RefreshTime ->
			%disk_cache_server:set_not_visited(Url);
			%io:format("refreshing ~p ~p ~p ~n", [Url, RefreshTime, CurrentTime]),
			visited_urls_server:insert(common:get_param(id, OldParams)); % TODO - sprawdzic czy adres nie ląduje kilkukrotnie w bazie
		true ->
			false
	end.

%% @private	
reportLoad(State) ->
	{Total,Allocated,_Worst} = memsup:get_memory_data(),
	case State#state.load_manager_counter of
		0 -> %initial load
			MemUsage = 0,
			CPULoad = 0,
			AvgTime = 0;
		N ->
			MemUsage = Allocated*100/Total,
			CPULoad = cpu_sup:avg1()*100/256, %% 0 - 100
			%AvgTime = (common:timestamp()-State#state.load_manager_timestamp)/1000/N
			AvgTime = processing_time_server:get_mean_time()/1000
	end,
	
	Params = common:stick_params({cpu, CPULoad},
		common:stick_params({memory, MemUsage},
			common:stick_params({avgTime, AvgTime}, [])
		)
	),
	
	case application:get_env(session_manager,domain_manager_node) of
		{ok, Node} ->
			rpc:call(Node, load_collector_server, report_load, [node(), Params]);
		undefined ->
			load_collector_server:report_load(node(), Params)
	end,
	ok.
		
%% @private	
handle_info(_Msg,State) ->
	{noreply,State}.

%% @private	
terminate(_Reason,_State) ->
	ok.

%% @private	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
