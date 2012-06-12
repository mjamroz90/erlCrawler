-module(url_processing).

-export([start_link/2, process/2]).

start_link(Id, Url) ->
	Pid = spawn_link(?MODULE, process, [Id, Url]),
	{ok, Pid}.


process(Id, Url) ->
	io:format("~p ~p ~n", [Id, Url]),
	timer:sleep(5000),
	case Id of
		1 ->
			erlang:raise(exit, "Boom!", []);
		_ ->
			ok
	end,
	io:format("END ~p ~p ~n", [Id, Url]),
	
	
	Urls = parse(Id, Url),
	process_urls(Urls, url_server:lookup(Url), reg:get_domain(Url)),
	scheduler:completed(),
	ok.


parse(Id, Url)->
	io:format("parsing ~p ~p ~n", [Id, Url]),
	[Url, Url ++ "1"].


process_urls([], _RefParams, _RefDomain) -> ok;
process_urls([Url | T], RefParams, RefDomain) ->
	[{timestamp, _RefTimestamp}, {width, RefWidth}, {depth, RefDepth}] = RefParams, 
	case reg:get_domain(Url) of
		RefDomain ->
			Width = RefWidth,
			Depth = RefDepth-1;
			
		_Other ->
			Width = RefWidth-1,
			Depth = RefDepth
	end,
	
	case url_server:lookup(Url) of
		not_found ->		
			if
				(Width >= 0) and (Depth >= 0) ->
					Params = [{timestamp, 0}, {width, Width}, {depth, Depth}],
					url_server:insert(Url, Params);
				true ->
					false
			end;
		ExistingParams ->
			[{timestamp, ExistingTimestamp}, {width, ExistingWitdh}, {depth, ExistingDepth}] = ExistingParams,
			if
				ExistingWitdh > Width -> %bylismy blizej
					true;
				((Width > ExistingWitdh) and (Depth >= 0)) or ((Width == ExistingWitdh) and (Depth > ExistingDepth)) ->
					%jestesmy w mniejszej szerokosci badz w tej samej, ale na mniejszej glebokosci
					Params = [{timestamp, ExistingTimestamp}, {width, Width}, {depth, Depth}],
					url_server:update(Url, Params);
				true ->
					false
			end
			%TODO - sprawdzic timestamp, moze czas odswiezyc			
	end,
	process_urls(T, RefParams, RefDomain).
