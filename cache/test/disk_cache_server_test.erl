-module(disk_cache_server_test).
-include_lib("eunit/include/eunit.hrl").
-define(NODENAME,"127.0.0.1").
-define(PORT,8087).

disk_cache_server_test_() ->
	{setup,
	fun setup/0,
	fun cleanup/1,
	[
		fun test_function/0
	]
	}.
	
setup() ->
	disk_cache_server:start(?NODENAME,?PORT).
	
cleanup(_) ->
	disk_cache_server:stop().
	
test_function() ->
	Seq = lists:seq(1,100),
	lists:foreach(fun(X) -> disk_cache_server:insert(make_url(X),{{width,X},{depth,X*2}}) end, Seq),
	lists:foreach(fun(X) -> ?assertEqual({{width,X},{depth,X*2}},disk_cache_server:lookup(make_url(X))) end, Seq),
	?assertEqual(not_found,disk_cache_server:lookup("www.abcd.pl")),
	lists:foreach(fun(X) -> disk_cache_server:update(make_url(X),{{width,2*X},{depth,X}}) end, Seq),
	lists:foreach(fun(X) -> ?assertEqual({{width,2*X},{depth,X}},disk_cache_server:lookup(make_url(X))) end, Seq),
	lists:foreach(fun(X) -> disk_cache_server:delete(make_url(X)) end, Seq),
	lists:foreach(fun(X) -> ?assertEqual(not_found,disk_cache_server:lookup(make_url(X))) end, Seq).	
	
make_url(Num) ->
	"www.agh.edu.pl"++lists:flatten(io_lib:format("~p", [Num])).
