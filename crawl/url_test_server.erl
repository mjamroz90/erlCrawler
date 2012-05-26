-module(url_test_server).
-compile(export_all).

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
								disk_cache_server:delete(Url)
							 catch
								_:_ -> void
							 end	
								 end,List),
	io:format("Usunalem ~p\n",[Ins+Size]),
	remove_partially(Ins+Size,Size,Max,Descr).

test_insert_disk(Filename,Max,Size) ->
	test(Filename,Max,Size,fun(List) -> time_insert_list_disk(List) end).

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
	[{Ins+Size,{Time_ins,Time_lookup}} | test(Ins+Size,Max,Size,Descr,F)].
	
start() ->
	application:start(cache).

stop() ->
	application:stop(cache).

%%time_lookup_list(List) ->
%%	{Time,_} = timer:tc(fun() -> lookup(List) end),	
%%	{{wholeTime,Time/1000000},{avgTime,Time/(length(List)*1000)}}.
	
%%time_delete(FileName,Num) ->
%%	{Time,_} = timer:tc(fun()->delete(FileName,Num) end),
%%	{{wholeTime,Time},{avgTime,Time/Num}}.
	
%%time_insert(FileName,Num) ->
%%	{Time,_} = timer:tc(fun()->insert(FileName,Num) end),
%%	{{wholeTime,Time/1000000},{avgTime,Time/(Num*1000)}}.

time_insert_list_disk(List) ->
	{Time_ins,_} = timer:tc(fun()->insert_disk(List) end),
	{Time_lookup,_} = timer:tc(fun() -> lookup_disk(List) end),
	{{wholeTime,Time_ins/1000000},{avgTime,{Time_ins/(length(List)*1000),Time_lookup/(length(List)*1000)}}}.
	
time_insert_list_ram(List) ->
	{Time_ins,_} = timer:tc(fun()->insert_ram(List) end),
	{Time_lookup,_} = timer:tc(fun() -> lookup_ram(List) end),
	{{wholeTime,Time_ins/1000000},{avgTime,{Time_ins/(length(List)*1000),Time_lookup/(length(List)*1000)}}}.	
	
%%insert(FileName,Num) ->
%%	L = get_list(FileName,Num),
%%	insert(L).
	
insert_disk(List) ->
	lists:foreach(fun(Url) -> try 
								disk_cache_server:insert(Url,{{width,2},{depth,3}}) 
							  catch
								 _:_ -> void
							  end
							  end,List).							 
	
insert_ram(List) ->
	lists:foreach(fun(Url) -> ram_cache_server:insert(Url,{{width,2},{depth,3}}) end,List).

lookup_disk(List) ->
	lists:foreach(fun(Url) -> disk_cache_server:lookup(Url) end,List).
	
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
	{ok,Descr} = file:open("crawl/ins_results",[write]),
	{ok,Descr1} = file:open("crawl/lookup_results",[write]),
	lists:foreach(fun({Num,{Time_ins,Time_lookup}}) ->  io:format(Descr,"~p\t~p\n",[Num,Time_ins]),
														io:format(Descr1,"~p\t~p\n",[Num,Time_lookup]) end,List),
	file:close(Descr),
	file:close(Descr1).
	
delete(FileName,Num) ->
	L = get_list(FileName,Num),
	lists:foreach(fun(Url) -> disk_cache_server:delete(Url) end,L).

