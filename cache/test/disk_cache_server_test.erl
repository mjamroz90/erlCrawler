-module(disk_cache_server_test).
-include_lib("eunit/include/eunit.hrl").

disk_cache_server_test_() ->
	{setup,
	fun setup/0,
	fun cleanup/1,
	[
		fun test_function/0,
		fun pull_test_function/0
	]
	}.
	
setup() ->
	application:start(crawl_event),
	application:start(cache).
	
cleanup(_) ->
	application:stop(cache),
	application:stop(crawl_event).
	
test_function() ->
	Seq = lists:seq(1,1000),
	lists:foreach(fun(X) -> disk_cache_server:insert(make_url(X),[{width,X},{depth,X*2}]) end, Seq),
	lists:foreach(fun(X) -> [_Id | T] = disk_cache_server:lookup(make_url(X)), ?assertEqual([{width,X},{depth,X*2}],T) end, Seq),
	?assertEqual(not_found,disk_cache_server:lookup("www.abcd.pl")),
	lists:foreach(fun(X) -> disk_cache_server:update(make_url(X),[{width,2*X},{depth,X}]) end, Seq),
	lists:foreach(fun(X) -> [_Id | T] = disk_cache_server:lookup(make_url(X)), ?assertEqual([{width,2*X},{depth,X}],T) end, Seq),
	lists:foreach(fun(X) -> disk_cache_server:delete(make_url(X)) end, Seq),
	lists:foreach(fun(X) -> ?assertEqual(not_found,disk_cache_server:lookup(make_url(X))) end, Seq).
	
pull_test_function() ->
	?assertEqual([], disk_cache_server:pull_urls(1)),
	lists:foreach(fun(X) -> visited_urls_server:insert(1) end, lists:seq(1,5)),	
	?assertEqual([{1, not_found}], disk_cache_server:pull_urls(5)),
	?assertEqual([], disk_cache_server:pull_urls(5)),
	{ok, [{id, Id} | _T]} = disk_cache_server:insert(make_url(0),[]),
	visited_urls_server:insert(Id),
	?assertEqual([{Id, make_url(0)}], disk_cache_server:pull_urls(5)),
	?assertEqual([], disk_cache_server:pull_urls(5)).
	
	
make_url(Num) ->
	"www.agh.edu.pl"++lists:flatten(io_lib:format("~p", [Num])).
