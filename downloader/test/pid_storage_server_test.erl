-module(pid_storage_server_test).
-include_lib("eunit/include/eunit.hrl").

pid_storage_server_test_() ->
	{setup,
	 fun setup/0,
	 fun cleanup/1,
	 [
		fun test_insert/0
	 ]
	}.

setup() ->
	pid_storage_server:start().


test_insert() ->
	PidList = lists:map(fun(Num) -> list_to_pid("<0."++integer_to_list(Num)++".0>") end,lists:seq(1,100)),
	lists:foreach(fun(Pid) -> pid_storage_server:insert(Pid) end,PidList),
	?assertEqual(100,pid_storage_server:get_size()).


cleanup(_) ->
	pid_storage_server:stop().

