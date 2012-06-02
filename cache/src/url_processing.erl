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
	process_urls(Urls),
	scheduler:completed(),
	ok.


parse(Id, Url)->
	io:format("parsing ~p ~p ~n", [Id, Url]),
	["Url", "Url" ++ "1"].


process_urls([]) -> ok;
process_urls(_Urls) ->
	ok.
