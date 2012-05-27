-module(ram_cache_server_test).
-include_lib("eunit/include/eunit.hrl").

ram_cache_server_test_() ->
	{setup,
	 fun setup/0,
	 fun cleanup/1,
	 [
		fun test_function/0
	 ]
	}.
	
test_function() ->
	Seq = lists:seq(1,1000),
	lists:foreach(fun(X) -> ram_cache_server:insert(X,2*X) end,Seq),
	lists:foreach(fun(X) -> ?assertEqual(ram_cache_server:lookup(X),2*X) end,Seq),
	Seq1 = lists:seq(1001,1500),
	lists:foreach(fun(X) -> ram_cache_server:insert(X,2*X) end,Seq1),
	?assertEqual(ram_cache_server:get_cache_size(),1000),
	Seq2 = lists:seq(1,1500),
	lists:foreach(fun(X) -> ram_cache_server:delete(X) end,Seq2),
	?assertEqual(ram_cache_server:get_cache_size(),0).
	
setup() ->
	ram_cache_server:start(1000).
	
cleanup(_) ->
	ram_cache_server:stop().