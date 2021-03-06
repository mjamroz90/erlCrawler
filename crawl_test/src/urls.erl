-module(urls).
-compile(export_all).
-define(TESTED_MOD,disk_cache_server).
-define(TESTED_APP,cache).

remove_partially(Size,Max) ->
	start(),
	{ok,Descr} = file:open("crawl/urls2",[read]),
	remove_partially(0,Size,Max,Descr),
	file:close(Descr),
	stop().
	
remove_partially(Max,_,Max,_) -> 
	ok;
	
remove_partially(Ins,Size,Max,Descr) ->
	List = get_list1(Descr,Size),
	lists:foreach(fun(Url) ->try
								?TESTED_MOD:delete(Url)
							 catch
								_:_ -> void
							 end	
								 end,List),
	io:format("Usunalem ~p\n",[Ins+Size]),
	remove_partially(Ins+Size,Size,Max,Descr).

test_insert_disk_domain(FileName,Max,Size) ->	
		url_test_server:start(),
		domain_manager_app:do_once(),
		mnesia:start(),
		domain_manager_app:do_once1(),
		domain_dispatch_server:start([node()]),
		test(FileName,Max,Size,fun(List) -> time_insert_list_domain(List) end),
		url_test_server:stop().
	
test_insert_disk_url(Filename,Max,Size) ->	
		spawn(fun() -> 
		url_test_server:start(),
		test(Filename,Max,Size,fun(List) -> time_insert_list_disk(List) end),
		url_test_server:stop() end).
		
%% ======================CONCURRENT===========================================
test_insert_disk_conc(FileName,Max,ThreadNum) ->
	{ok,Descr} = file:open(FileName,[read]),
	start(),
	Lists = lists:map(fun(_) -> get_list1(Descr,Max) end,lists:seq(1,ThreadNum)),
	MyPid = self(),
	Threads = lists:map(fun(Num) -> spawn(fun() -> thread_action(Num,MyPid) end) end,lists:seq(1,ThreadNum)),
	lists:foldl(fun(Pid,Num) -> Pid ! {start,lists:nth(Num,Lists)}, Num+1 end,1,Threads),
	{Time,Result} = timer:tc(fun() -> wait_for_finish(ThreadNum,[]) end),
	file:close(Descr),
	stop(),
	{Time/1000,Result}.
	
wait_for_finish(ThreadNum,ExecThreads) ->
	receive
		
		{Num,Time} ->
					  NewList = [{Num,Time} | ExecThreads],
					  io:format("COLLECTOR - Otrzymalem wynik od ~p\n",[Num]),
					  case lists:sort(lists:map(fun({Num1,_}) -> Num1 end,NewList)) =:= lists:seq(1,ThreadNum) of
						true -> NewList;
						false -> wait_for_finish(ThreadNum,NewList)
					  end
	end.

thread_action(Num,CollectorPid) ->
	receive
		{start,List} -> ok
	end,	
	{Time,_} = timer:tc(fun() -> insert_disk(List) end),
	CollectorPid ! {Num,Time/1000}.
	
%% ======================CONCURRENT===========================================
	
test_insert_ram(Filename,Max,Size) ->
	test(Filename,Max,Size,fun(List) -> time_insert_list_ram(List) end).
	
test(Filename,Max,Size,F) ->
	start(),
	{ok,Descr} = file:open(Filename,[read]),
	List = test(0,Max,Size,Descr,F),
	write_to_file(List),	
	file:close(Descr),
	stop().

test(Max,Max,_,_,_) ->
	[];
	
test(Ins,Max,Size,Descr,F) ->	
	List = get_list1(Descr,Size),
	io:format("Zdobylem liste\n"),
	{_,{avgTime,{Time_ins,Time_lookup}}} = F(List),
	io:format("Po operacji na ~p\n",[Ins]),	
	url_test_server:put_result({Ins+Size,{Time_ins,Time_lookup}}),
	[{Ins+Size,{Time_ins,Time_lookup}} | test(Ins+Size,Max,Size,Descr,F)].
	
start() ->
	application:start(?TESTED_APP).

stop() ->
	application:stop(?TESTED_APP).

time_insert_list_domain(List) ->
	{Time_ins,_} = timer:tc(fun()->insert_domain(List) end),
	{Time_lookup,_} = timer:tc(fun() -> lookup_domain(List) end),
	{{wholeTime,Time_ins/1000000},{avgTime,{Time_ins/(length(List)*1000),Time_lookup/(length(List)*1000)}}}.
	
time_insert_list_disk(List) ->
	{Time_ins,_} = timer:tc(fun()->insert_disk(List) end),
	{Time_lookup,_} = timer:tc(fun() -> lookup_disk(List) end),
	{{wholeTime,Time_ins/1000000},{avgTime,{Time_ins/(length(List)*1000),Time_lookup/(length(List)*1000)}}}.
	
time_insert_list_ram(List) ->
	{Time_ins,_} = timer:tc(fun()->insert_ram(List) end),
	{Time_lookup,_} = timer:tc(fun() -> lookup_ram(List) end),
	{{wholeTime,Time_ins/1000000},{avgTime,{Time_ins/(length(List)*1000),Time_lookup/(length(List)*1000)}}}.	
	
insert_disk(List) ->
	[H | T] = List,
	?TESTED_MOD:insert(H,[{width,2},{depth,3}]),	
	lists:foreach(fun(Url) -> try 
								?TESTED_MOD:insert(Url,[{width,2},{depth,3}]) 
							  catch
								 _:_ -> void
							  end
					end,T).

insert_domain(List) ->
	lists:foreach(fun(Domain) -> domain_dispatch_server:insert(Domain) end,List).
	
lookup_domain(List) ->
	lists:foreach(fun(Url) -> domain_cache_server:lookup(Url) end,List).
	
insert_ram(List) ->
	lists:foreach(fun(Url) -> ram_cache_server:insert(Url,[{width,2},{depth,3}]) end,List).

lookup_disk(List) ->
	lists:foreach(fun(Url) -> ?TESTED_MOD:lookup(Url) end,List).
	
lookup_ram(List) ->
	lists:foreach(fun(Url) -> ram_cache_server:lookup(Url) end,List).

%=======================================================================
	
get_list(FileName,Num) ->
	case file:open(FileName,[read]) of
		{ok,Descr} ->			
			get_list1(Descr,Num);
		Err ->
			Err
	end.
	
get_list1(_,0) ->
	%file:close(Descr),
	[];
	
get_list1(Descr,Num) ->
	[io:get_line(Descr,"") | get_list1(Descr,Num-1)].

prepare(Filename,Num) ->
	case file:open(Filename,[read]) of
		{ok,Descr} ->
			{ok,Descr1} = file:open(prepare_file(Num),[write]),
			read_write(Descr,Descr1,Num);
		Err ->
			Err
	end.
	
read_write(Descr,Descr1,0) ->
	file:close(Descr),
	file:close(Descr1);
	
read_write(Descr,Descr1,Num) ->
	case io:get_line(Descr,"") of		
		eof -> 	file:close(Descr),
				file:close(Descr1);
				
		Data -> process_data(Data,Descr1),
				read_write(Descr,Descr1,Num-1)
	end.	

prepare_file(Num) ->
	"urls"++lists:flatten(io_lib:format("~p", [Num])).

process_data(Data,Descr1) ->
	case lists:member(Data,["\n","","0"]) of 		
		false -> io:format(Descr1,"~p\n",[Data]);
		_ -> void
	end.	
	
write_to_file(List) ->
	{ok,Descr} = file:open("ins_results",[write]),
	{ok,Descr1} = file:open("lookup_results",[write]),
	lists:foreach(fun({Num,{Time_ins,Time_lookup}}) ->  io:format(Descr,"~p\t~p\n",[Num,Time_ins]),
														io:format(Descr1,"~p\t~p\n",[Num,Time_lookup]) end,List),
	file:close(Descr),
	file:close(Descr1).
	
delete(FileName,Num) ->
	L = get_list(FileName,Num),
	lists:foreach(fun(Url) -> ?TESTED_MOD:delete(Url) end,L).

