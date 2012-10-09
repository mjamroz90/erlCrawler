-module(mockparser).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/1, get_addr/0, stop/0, get_list/1, mockparse/2]).

%============================API==================================     
start(Filename) ->
	gen_server:start_link({local,?MODULE},?MODULE,[Filename], []).
	
get_addr() ->
	gen_server:call(?MODULE,get_addr).
	
stop() ->
	gen_server:cast(?MODULE,stop).
	
mockparse(_Id, _Url) ->
	get_many_addr(random:uniform(80)).

%============================CallBacks============================
init([Filename]) ->	
	{ok,get_list(Filename)}.
	
handle_cast(stop,State) ->
	{stop,"Made to stop",State}.
	
handle_call(get_addr,_From,State) ->
	%Index = random:uniform(length(State)),
	%Reply = lists:nth(Index, State),
	[H | T] = State,
	Reply = H,
	{reply,Reply,T}.

handle_info(_Msg,State) ->
	{ok,State}.
	
terminate(_Reason,_State) ->
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.


get_list(FileName) ->
	case file:open(FileName,[read]) of
		{ok,Descr} ->			
			get_list1(Descr);
		Err ->
			Err
	end.
	
get_list1(Descr) ->
	case io:get_line(Descr,"") of
		eof -> [];
		Data -> 
			[Data | get_list1(Descr)]
	end.
	
get_many_addr(0) -> [];
get_many_addr(N) -> [get_addr() | get_many_addr(N-1)].
