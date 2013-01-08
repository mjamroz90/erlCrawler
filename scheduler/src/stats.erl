-module(stats).
-export([start/2, report/1, log_stats_to_web/0, get_total_counter/0, get_part_counter/0, get_mean_speed/0, get_part_mean_speed/0,
		get_total_urls_counter/0, get_part_urls_counter/0, get_mean_urls_counter/0, get_part_mean_urls_counter/0,
		get_percentage_cpu_load/0, get_percentage_memory_load/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         
-record(state,{file_descr, start_time, total_counter, part_size, part_start_time, part_counter, part_mean_speed,
		total_urls_counter, part_urls_counter, part_mean_urls_counter}).
    
-define(URL_BUCKET,term_to_binary("Urls")).
    
%======================================API==================================

%% @spec start(LogToFile :: string()/false, PartSize :: int()) -> {ok, Pid} | {error,term()}
%% @doc Uruchamia serwer zbierajacy statystyki, argumenty: <dl>
%% <dt>LogToFile</dt><dd>nazwa pliku zapisu statystyk lub false</dd>
%% <dt>PartSize</dt><dd>ilosc stron dla pomiaru chwilowego</dd>
%% </dl>
%% @end         
start(LogToFile, PartSize) ->
	NewPartSize = case PartSize of
		0 -> 1000;
		N -> N
	end,
	gen_server:start_link({local,?MODULE},?MODULE,[LogToFile,NewPartSize],[]).
	
%% @spec report(Proplist :: proplist()) -> ok
%% @doc raportuje przetworzenie strony do serwera, Proplist powinna zawierac parametr urls_counter opisujacy liczbe adresow uzyskanych z przetworzonej strony.
report(Proplist) ->
	gen_server:cast(?MODULE, {report, Proplist}).
	
%% @spec log_stats_to_web() -> ok
%% @doc Wysyla zebrane statystyki do aplikacji webowej
log_stats_to_web() ->
	gen_server:cast(?MODULE, log_stats_to_web).
	
%% @spec get_total_counter() -> int()
%% @doc Zwraca ogolna liczbe przetworzonych stron.
get_total_counter() ->
	gen_server:call(?MODULE, get_total_counter).

%% @spec get_part_counter() -> int()
%% @doc Zwraca liczbe przetworzonych stron w aktualnej malej probce.
get_part_counter() ->
	gen_server:call(?MODULE, get_part_counter).
	
%% @spec get_mean_speed() -> double()
%% @doc Zwraca sredni czas potrzebny na przetworzenie strony (w mikrosekundach).
get_mean_speed() ->
	gen_server:call(?MODULE, get_total_mean_speed).
	
%% @spec get_part_mean_speed() -> double()
%% @doc Zwraca sredni chwilowy czas potrzebny na przetworzenie strony (w mikrosekundach).
get_part_mean_speed() ->
	gen_server:call(?MODULE, get_part_mean_speed).

%% @spec get_total_urls_counter() -> int()
%% @doc Zwraca liczbe adresow uzyskanych z przetworzonych stron.
get_total_urls_counter() ->
	gen_server:call(?MODULE, get_total_urls_counter).
	
%% @spec get_part_urls_counter() -> int()
%% @doc Zwraca liczbe adresow uzyskanych z przetworzonych stron aktualnej malej probki.
get_part_urls_counter() ->
	gen_server:call(?MODULE, get_part_urls_counter).

%% @spec get_mean_urls_counter() -> double()
%% @doc Zwraca srednia liczbe urli na stronie.
get_mean_urls_counter() ->
	gen_server:call(?MODULE, get_mean_urls_counter).
	
%% @spec get_part_mean_urls_counter() -> double()
%% @doc Zwraca srednia chwilowa liczby adresow uzyskanych z przetworzonych stron.
get_part_mean_urls_counter() ->
	gen_server:call(?MODULE, get_part_mean_urls_counter).
	
%% @spec get_percentage_cpu_load() -> double()
%% @doc Zwraca obciazenie procesora w skali 0 - 100*liczba_rdzeni.
get_percentage_cpu_load() ->
	get_percentage_cpu_load1().
	
%% @spec get_percentage_memory_load() -> double()
%% @doc Zwraca wykorzystanie pamieci w skali 0 - 100.
get_percentage_memory_load() ->
	get_percentage_memory_load1().
	


%% @spec stop() -> ok
%% @doc Zatrzymuje serwer.
stop() ->
	gen_server:cast(?MODULE,stop).
	
%===================================Callbacks==============================

%% @private
init([LogToFile, PartSize]) ->
	Descr = case LogToFile of
		false -> false ;
		Filename ->
			case file:open(Filename, [write]) of
				{ok, IoDevice} ->
					IoDevice;
				{error, _Reason} ->
					false
			end
		
	end,
	timer:apply_interval(60000,?MODULE, log_stats_to_web, []),
	{ok,#state{file_descr = Descr, start_time = 0, total_counter = 0,
		part_size = PartSize, part_start_time = 0, part_counter = 0, part_mean_speed = 0,
		total_urls_counter = 0, part_urls_counter = 0, part_mean_urls_counter = 0 }}.


%% @private		
handle_cast(stop,State) ->
	{stop,"Made to stop",State};

handle_cast({report, Proplist}, State = #state{file_descr = Descr, start_time = StartTime, total_counter = TotalCounter,
	part_size = PartSize, part_start_time = PartStartTime, part_counter = PartCounter, part_mean_speed = PartMeanSpeed,
	total_urls_counter = TotalUrlsCounter, part_urls_counter = PartUrlsCounter, part_mean_urls_counter = PartMeanUrlsCounter}) ->
	
	%rozpoczynamy liczenie statystyk
	case TotalCounter of
		0 ->
			NewStartTime = common:timestamp();
		_ ->
			NewStartTime = StartTime
	end,
	
	%rozpoczynamy liczenie statystyk czesciowych
	case PartCounter of
		0 ->
			NewPartStartTime = common:timestamp();
		_ ->
			NewPartStartTime = PartStartTime
	end,
	

	CurrentUrlsCounter = case common:get_param(urls_counter, Proplist) of
		not_found -> 0;
		N -> N
	end,
	NewTotalUrlsCounter = TotalUrlsCounter + CurrentUrlsCounter,
	
	%zebralismy statystyki z PartSize przetworzen, przygotowujemy do zapisu i zapisujemy	
	case PartCounter + 1 of
		PartSize ->
			NewPartMeanSpeed = (common:timestamp() - PartStartTime)/PartSize,
			TotalMeanSpeed = (common:timestamp() - StartTime)/(TotalCounter+1),
			
			NewMeanUrlsCounter = NewTotalUrlsCounter/(TotalCounter+1),
			NewPartMeanUrlsCounter = (PartUrlsCounter+CurrentUrlsCounter)/PartSize,
			NewPartUrlsCounter = 0,
			
			log_to_file(Descr, TotalCounter+1, TotalMeanSpeed, NewPartMeanSpeed, NewTotalUrlsCounter, NewMeanUrlsCounter, NewPartMeanUrlsCounter);
		_ -> 
			NewPartMeanSpeed = PartMeanSpeed,
			NewPartMeanUrlsCounter = PartMeanUrlsCounter,
			NewPartUrlsCounter = PartUrlsCounter + CurrentUrlsCounter
	end,
	
	{noreply, State#state{start_time = NewStartTime, total_counter = TotalCounter + 1, part_start_time = NewPartStartTime,
		part_counter = (PartCounter + 1) rem PartSize, part_mean_speed = NewPartMeanSpeed,
		total_urls_counter = NewTotalUrlsCounter, part_urls_counter = NewPartUrlsCounter, part_mean_urls_counter = NewPartMeanUrlsCounter}};


handle_cast(log_stats_to_web, State = #state{start_time = StartTime, total_counter = TotalCounter,
	part_mean_speed = PartMeanSpeed, total_urls_counter = TotalUrlsCounter, part_mean_urls_counter = PartMeanUrlsCounter}) ->
	
	TotalMeanSpeed = case TotalCounter of
		0 -> 0;
		N -> 1000000/((common:timestamp() - StartTime)/N)
	end,
	
	TotalMeanUrlsCounter = case TotalCounter of
		0 -> 0;
		N2 -> TotalUrlsCounter/N2
	end,
	
	PartMeanSpeed2 = case PartMeanSpeed of
		0 -> 0;
		N3 -> 1000000/N3
	end,
	
	log_to_web(TotalCounter, TotalMeanSpeed, PartMeanSpeed2, TotalUrlsCounter, TotalMeanUrlsCounter, PartMeanUrlsCounter),
	{noreply, State}.
		
			
%% @private	
handle_call(get_total_counter, _From, State = #state{total_counter = TotalCounter}) ->
	{reply,TotalCounter,State};
	
handle_call(get_part_counter, _From, State = #state{part_counter = PartCounter}) ->
	{reply, PartCounter, State};
	
handle_call(get_total_mean_speed, _From, State = #state{total_counter = TotalCounter, start_time = StartTime}) ->
	MeanSpeed = case TotalCounter of
		0 ->
			0;
		N ->
			(common:timestamp()-StartTime)/TotalCounter
	end,
	{reply, MeanSpeed, State};
	
handle_call(get_part_mean_speed, _From, State = #state{part_mean_speed = PartMeanSpeed}) ->
	{reply, PartMeanSpeed, State};
	
	
handle_call(get_total_urls_counter, _From, State = #state{total_urls_counter = TotalUrlsCounter}) ->
	{reply, TotalUrlsCounter, State};
	
handle_call(get_part_urls_counter, _From, State = #state{part_urls_counter = PartUrlsCounter}) ->
	{reply, PartUrlsCounter, State};
	
handle_call(get_mean_urls_counter, _From, State = #state{total_urls_counter = TotalUrlsCounter, total_counter = TotalCounter}) ->
	MeanUrlsCounter = case TotalCounter of
		0 ->
			0;
		N ->
			TotalUrlsCounter/N
	end,
	{reply, MeanUrlsCounter, State};
	
handle_call(get_part_mean_urls_counter, _From, State = #state{part_mean_urls_counter = PartMeanUrlsCounter}) ->
	{reply, PartMeanUrlsCounter, State}.


%% @private			
handle_info(_Msg,State) ->
	{noreply,State}.
	
	
%% @private	
terminate(_Reason,#state{file_descr = Descr}) ->
	case Descr of
		false ->
			ok;
		IoDevice ->
			file:close(IoDevice)
	end.	


%% @private	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
	
%% @private
log_to_file(Descr, TotalCounter, TotalMeanSpeed, PartMeanSpeed, TotalUrlsCounter, TotalMeanUrlsCounter, PartMeanUrlsCounter) ->
	IoDevice = case Descr of
		false ->
			standard_io;
		Device ->
			Device
	end,
	%liczba przetworzonych stron+, sredni chwilowy czas przetwarzania strony, srednia chwilowa liczby stron na sekunde, sredni czas przetwarzania strony+, srednia chwilowa liczby stron na sekunde,
	%uzycie procesora+, uzycie pamieci+, liczba dotychczas uzyskanych adresow+, srednia chwilowa liczby adresow na stronie+, srednia liczba adresów na stronie+
	io:format(IoDevice,"~p ~p ~p ~p ~p ~p ~p ~p ~p ~p~n", [TotalCounter, PartMeanSpeed, 1000000/PartMeanSpeed, TotalMeanSpeed, 1000000/TotalMeanSpeed, get_percentage_cpu_load(), get_percentage_memory_load(), TotalUrlsCounter, PartMeanUrlsCounter, TotalMeanUrlsCounter]).


%% @private
log_to_web(TotalCounter,TotalMeanSpeed, PrtMeanSpeed, TotalUrlsCounter, TotalMeanUrlsCounter, PartMeanUrlCounter) ->
  Msg = [{nodeName,node()},{totalProcessedSitesNum,TotalCounter},{meanSiteProcessingNum,TotalMeanSpeed},{meanProcessorUsage,get_percentage_cpu_load()},
          {memoryUsage,get_percentage_memory_load()},{totalAddressesFetchedNum,TotalUrlsCounter},{meanAddressesNumPerSite,TotalMeanUrlsCounter},
          {partAddressesNumPerSite,PartMeanUrlCounter}
  ],
  crawl_event:report_stats(Msg,false).

%% @private
get_percentage_memory_load1() ->
	{Total,Allocated,_Worst} = memsup:get_memory_data(),
	Allocated*100/Total.


%% @private
get_percentage_cpu_load1() ->
	cpu_sup:avg1()*100/256. %% 0 - 100*num_cores	
	
