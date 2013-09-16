%% @doc Modul odpowiedzialny za uruchomienienie przetwarzania strony i przetworzenie adresow z niej zebranych
%% @end
-module(url_processing).

-export([start_link/2, start_link/4, process/2, process/4]).

%% @spec start_link(Id :: term(), Url :: string()) -> {ok, Pid}
%% @doc Uruchamia proces przetwarzajacy adres Url z identyfikatorem Id.
%% @end
start_link(Id, Url) ->
	Pid = spawn_link(?MODULE, process, [Id, Url]),
	{ok, Pid}.
	
%% @spec start_link(Id :: term(), Url :: string(), Source :: binary()) -> {ok, Pid}
%% @doc Uruchamia proces przetwarzajacy zrodlo Source spod adresu Url z identyfikatorem Id.
%% @end
start_link(Id, Url, RedirectedUrl, Source) ->
	Pid = spawn_link(?MODULE, process, [Id, Url, RedirectedUrl, Source]),
	{ok, Pid}.

%% @spec process(Id :: term(), Url :: string()) -> ok
%% @doc Aktualizuje date przetworzenia strony, uruchamia parsowanie, przetwarzanie adresow uzyskanych ze strony, powiadamia schedulera o zakonczeniu przetwarzania.
%% @end
process(Id, Url) ->
	%% TODO - integracja
	
	%io:format("~p ~p ~n", [Id, Url]),
	%timer:sleep(5000),
	%case Id of
		%1 ->
			%erlang:raise(exit, "Boom!", []);
		%_ ->
			%ok
	%end,
	%io:format("END ~p ~p ~n", [Id, Url]),
	
	%updating timestamp
	StartTime = common:timestamp(),
	Params = url_server:lookup(Url),
	NewParams = common:stick_params({timestamp, common:timestamp()}, Params),
	url_server:update(Url, NewParams),
	
	
	%Urls = parse(Id, Url), %%%%% interfejs do przetwarzania
	%Urls = url_extractor:extract(Id, Url),
	
	%Urls = mockparser:mockparse(Id, Url, 100),
	%io:format("parsing ~p ~p ~n", [Id, Url]),
	%Urls = mockparser:mockparse3(Id, Url, 100),
	Urls = processing_handler:process_data(Id, Url),
	
	stats:report(common:stick_params({urls_counter, length(Urls)}, [])),
	
	
	process_urls(Urls, url_server:lookup(Url), reg:get_domain(Url), reg:get_full_domain(Url)),
	scheduler:completed(),
	EndTime = common:timestamp(),
	
	Time = EndTime - StartTime,
	processing_time_server:report(Time),
	ok.
	

%process(Id, Url, _Source) ->
	%process(Id, Url).

	
process(Id, Url, RedirectedUrl, Source) ->	
	%process(Id, Url).
	StartTime = common:timestamp(),
	Params = url_server:lookup(Url),
	NewParams = common:stick_params({timestamp, common:timestamp()}, Params),
	url_server:update(Url, NewParams),
	
	Urls = processing_handler:process_data(Id, RedirectedUrl, Source),
	
	stats:report(common:stick_params({urls_counter, length(Urls)}, [])),
	
	process_urls(Urls, url_server:lookup(Url), reg:get_domain(Url), reg:get_full_domain(Url)),
	scheduler:completed(),
	EndTime = common:timestamp(),
	
	Time = EndTime - StartTime,
	processing_time_server:report(Time),
	ok.

%% @private
parse(Id, Url)->
	io:format("parsing ~p ~p ~n", [Id, Url]),
	[Url, Url ++ "2", Url ++ "/1"].

%% @private
process_urls([], _RefParams, _RefDomain, _RefFullDomain) -> ok;
process_urls([Url | T], RefParams, RefDomain, RefFullDomain) when length(Url) < 2000 ->
	RefWidth = common:get_param(width, RefParams),
	RefDepth = common:get_param(depth, RefParams),
	Domain = reg:get_domain(Url),
  {Width, Depth} = case Domain of
		RefDomain ->
      case reg:get_full_domain(Url) of
        RefFullDomain -> %identyczna domena
          {RefWidth, RefDepth-1};
        _OtherSubdomain -> %inna poddomena
          session_manager:get_subdomain_start_params(Domain)
      end;
			
		_OtherDomain ->
      DefBreadth = session_manager:get_default_breadth(),
      DefDepth = session_manager:get_default_depth(),
      if
        (RefWidth-1 > DefBreadth) ->
          {DefBreadth, DefDepth};
        (RefDepth > DefDepth) ->
          {RefWidth-1, DefDepth};
        true ->
          {RefWidth-1, RefDepth}
      end
	end,
	
	Params = common:stick_params({timestamp, 0},
		common:stick_params({width, Width},
			common:stick_params({depth, Depth}, [])
		)
	),
	
	if
		(Width >= 0) and (Depth >= 0) -> %czy jest sens przetwarzac
			case Domain of 
				RefDomain -> %ta sama domena - przetwarzanie na tym samym wezle
					scheduler:insert(Url, Params);
					
				OtherDomain -> %inna domena - sprawdzamy gdzie ma byc przetwarzany dany adres
					case domain_cache_server:lookup(OtherDomain) of
						not_found ->
							case application:get_env(session_manager,domain_manager_node) of
								{ok, Node} ->
									DestinationNode = rpc:call(Node, domain_dispatch_server, insert, [OtherDomain]);
								undefined ->
									DestinationNode = domain_dispatch_server:insert(OtherDomain)
							end;
							%DestinationNode = domain_dispatch_server:insert(OtherDomain);
						Node ->
							DestinationNode = Node
					end,
					if
						DestinationNode =:= node() ->
							scheduler:insert(Url, Params);
						true ->
							rpc:call(DestinationNode, scheduler, insert, [Url, Params])
					end
					%if
						%(Width >= 0) and (Depth >= 0) ->
							%rpc:call(DestinationNode, scheduler, insert, [Url, Params]);
						%true ->
							%false
					%end
			end;
		true ->
			false
	end,
	
	process_urls(T, RefParams, RefDomain, RefFullDomain);
	

process_urls([_Url | T], RefParams, RefDomain, RefFullDomain) ->
	process_urls(T, RefParams, RefDomain, RefFullDomain).
