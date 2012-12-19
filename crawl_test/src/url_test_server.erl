-module(url_test_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start/0,stop/0,get_current_results/0,put_result/1,write_to_file/2]).         

%============================API==================================

start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).
	
get_current_results() ->
	gen_server:call(?MODULE,get_current_results).
	
put_result(Tuple) ->
	gen_server:cast(?MODULE,{put_result,Tuple}).
	
stop() ->
	gen_server:cast(?MODULE,stop).
	
write_to_file(FileName1,FileName2) ->
	gen_server:cast(?MODULE,{write_to_file,{FileName1,FileName2}}).
	
%============================CallBacks============================

init([]) ->	
	{ok,[]}.
	
handle_cast({put_result,{Ins,{Time_ins,Time_lookup}}},State) ->
	{noreply,lists:append(State,[{Ins,{Time_ins,Time_lookup}}])};
	
handle_cast({write_to_file,{FileName1,FileName2}},State) ->
	write_to_file(FileName1,FileName2,State),
	{noreply,State};
		
handle_cast(stop,State) ->
	{stop,"Made to stop",State}.
	
handle_call(get_current_results,_From,State) ->
	{reply,State,State}.

handle_info(_Msg,State) ->
	{ok,State}.
	
terminate(_Reason,_State) ->
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

%=========================Internal===============================

write_to_file(FileName1,FileName2,State) ->	
	{ok,Descr} = file:open(FileName1,[write]),
	{ok,Descr1} = file:open(FileName2,[write]),
	lists:foreach(fun({Num,{Time_ins,Time_lookup}}) ->  io:format(Descr,"~p\t~p\n",[Num,Time_ins]),
														io:format(Descr1,"~p\t~p\n",[Num,Time_lookup]) end,State),
	file:close(Descr),
	file:close(Descr1).
