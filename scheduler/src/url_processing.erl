%% @doc Modul odpowiedzialny za uruchomienienie przetwarzania strony i przetworzenie adresow z niej zebranych
%% @end
-module(url_processing).

-export([start_link/2, process/2]).

%% @spec start_link(Id :: term(), Url :: string()) -> {ok, Pid}
%% @doc Uruchamia proces przetwarzajacy adres Url z identyfikatorem Id.
%% @end
start_link(Id, Url) ->
	Pid = spawn_link(?MODULE, process, [Id, Url]),
	{ok, Pid}.

%% @spec process(Id :: term(), Url :: string()) -> ok
%% @doc Aktualizuje date przetworzenia strony, uruchamia parsowanie, przetwarzanie adresow uzyskanych ze strony, powiadamia schedulera o zakonczeniu przetwarzania.
%% @end
process(Id, Url) ->
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
	stats:report(),
	%Urls = mockparser:mockparse(Id, Url, 100),
	%io:format("parsing ~p ~p ~n", [Id, Url]),
	Urls = mockparser:mockparse3(Id, Url, 100),
	
	
	
	process_urls(Urls, url_server:lookup(Url), reg:get_domain(Url)),
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
process_urls([], _RefParams, _RefDomain) -> ok;
process_urls([Url | T], RefParams, RefDomain) ->
	RefWidth = common:get_param(width, RefParams),
	RefDepth = common:get_param(depth, RefParams),
	Domain = reg:get_domain(Url),
	case Domain of
		RefDomain ->
			Width = RefWidth,
			Depth = RefDepth-1;
			
		_OtherDomain ->
			Width = RefWidth-1,
			Depth = RefDepth
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
	
	%case url_server:lookup(Url) of
		%not_found ->		
			%if
				%(Width >= 0) and (Depth >= 0) ->
					%%Params = [{timestamp, 0}, {width, Width}, {depth, Depth}],
					
					%url_server:insert(Url, Params);
				%true ->
					%false
			%end;
		%ExistingParams ->
			%scheduler:process_new_params(Url, ExistingParams, Params
			%%%[{timestamp, ExistingTimestamp}, {width, ExistingWitdh}, {depth, ExistingDepth}] = ExistingParams,
			%%ExistingTimestamp = common:get_param(timestamp, ExistingParams),
			%%ExistingWidth = common:get_param(width, ExistingParams),
			%%ExistingDepth = common:get_param(depth, ExistingParams),
			
			%%if
				%%ExistingWitdh > Width -> %bylismy blizej
					%%true;
				%%((Width > ExistingWitdh) and (Depth >= 0)) or ((Width == ExistingWitdh) and (Depth > ExistingDepth)) ->
					%%%jestesmy w mniejszej szerokosci badz w tej samej, ale na mniejszej glebokosci
					%%%Params = [{timestamp, ExistingTimestamp}, {width, Width}, {depth, Depth}],
					%%Params = common:stick_params({timestamp, ExistingTimestamp},
						%%common:stick_params({width, Width},
							%%common:stick_params({depth, Depth}, ExistingParams)
						%%)
					%%),
					%%url_server:update(Url, Params);
				%%true ->
					%%false
			%%end,
			
			%%%TODO - sprawdzic timestamp, moze czas odswiezyc
			%%ValidityTime = session_manager:get_validity_time(Domain),
			%%RefreshTime = ExistingTimestamp + ValidityTime,
			%%CurrentTime = common:timestamp(),
			%%if
				%%CurrentTime > RefreshTime ->
					%%disk_cache_server:set_not_visited(Url);
				%%true ->
					%%false
			%%end
				
			
						
	%end,
	process_urls(T, RefParams, RefDomain).
