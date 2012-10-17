-module(mockparser).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/2, get_addr/0, stop/0, get_list/1, mockparse/2, mockparse/3, mockparse2/3]).

%============================API==================================     
start(Filename, ReadBefore) ->
	gen_server:start_link({local,?MODULE},?MODULE,[Filename, ReadBefore], []).
	
get_addr() ->
	gen_server:call(?MODULE,get_addr).
	
stop() ->
	gen_server:cast(?MODULE,stop).
	
mockparse(_Id, _Url) ->
	get_many_addr(random:uniform(80)).
	
mockparse(_Id, _Url, Num) ->
	get_many_addr(Num).
	
%returns urls from the same domain
mockparse2(_Id, _Url, 0) ->
	[];
mockparse2(Id, Url, Num) ->
	["http://www.allegro.pl/" ++ integer_to_list(random:uniform(999999999999999999999999)) | mockparse2(Id, Url, Num-1)].

%============================CallBacks============================
init([Filename, ReadBefore]) ->	
	random:seed(),
	case ReadBefore of
		true ->
			{ok,get_list(Filename)};
		false ->
			case file:open(Filename,[read]) of
				{ok,Descr} ->			
					{ok, {descr, Descr}};
				_Err ->
					{ok, []}
			end
	end.
			
	
handle_cast(stop,State) ->
	{stop,"Made to stop",State}.
	
handle_call(get_addr,_From,State) ->
	%Index = random:uniform(length(State)),
	%Reply = lists:nth(Index, State),
	case State of
		{descr, Descr} ->
			Reply = get_url(Descr),
			{reply, Reply, State};
		[H | T] ->
			Reply = H,
			{reply,Reply,T}
	end.
	

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
	
get_url(Descr) -> 
	case io:get_line(Descr,"") of
		eof -> [];
		Data -> 
			Data
	end.

get_many_addr(0) -> [];
get_many_addr(N) -> [get_addr() | get_many_addr(N-1)].
