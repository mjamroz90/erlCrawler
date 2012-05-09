-module(ram_cache).
-export([new/1,insert/3,delete/2,lookup/2,test/0,exists/2,remove_random/1,remove/1,tab_size/1,delete_all/1, delete_old/1]).

test() ->
	Tab = new(?MODULE),
	List = lists:seq(1,20),
	lists:foreach(fun(X) -> insert(Tab,X,2*X) end,List),
	io:format("klucz 20 - wartosc ~p\n",[lookup(Tab,20)]),
	delete(Tab,20),
	io:format("klucz 20 - wartosc ~p\n",[lookup(Tab,20)]).
	
new(TabName) ->
	ets:new(TabName,[set]).
	
remove(Tab) ->
	ets:delete(Tab).
	
%insert(Tab,Url,Value) ->
%	ets:insert(Tab,{Url,Value}).
insert(Tab,Url,Value) ->
	ets:insert(Tab,{Url,{timestamp(),Value}}).
	
delete(Tab,Url) ->
	ets:delete(Tab,Url).

delete_all(Tab) ->
	Size = tab_size(Tab),
	remove_random(Tab,Size).	
	
%lookup(Tab,Url) ->	
%	case ets:lookup(Tab,Url) of
%		[] -> [];
%		[{_,Value}] -> Value
%	end.	
	
lookup(Tab,Url) ->	
	case ets:lookup(Tab,Url) of
		[] -> [];
		[{_,{_,Value}}] -> Value
	end.

insertion_timestamp(Tab, Url) ->
	case ets:lookup(Tab, Url) of
		[] -> [];
		[{_Key,{Timestamp,_Value}}] -> Timestamp
	end.

exists(Tab,Url) ->
	case lookup(Tab,Url) of 
		[] -> false;
		_ -> true
	end.

remove_random(Tab) ->
	remove_random(Tab,1).

tab_size(Tab) ->
	ets:info(Tab,size).
	
timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.
	
%=========================================================================	

remove_random(Tab,Num) ->	
	Key = ets:first(Tab),
	delete_random_keys(Tab,Key,0,Num).
	
delete_random_keys(_,_,Num,Num) ->
	ok;
	
delete_random_keys(Tab,Key,CurrNum,Num) ->
	Key1 = ets:next(Tab,Key),
	delete(Tab,Key),	
	delete_random_keys(Tab,Key1,CurrNum+1,Num).
	

%usuwa wszystkie dane z przedzialu czasu (najstarszy, najstarszy+Microseconds)
delete_old(Tab) ->
	Microseconds = 1000000,
	delete_old(Tab, ets:first(Tab), find_oldest_timestamp(Tab,ets:first(Tab),timestamp())+Microseconds).
	
delete_old(_Tab, '$end_of_table', _Timestamp) -> ok;
delete_old(Tab, Key, Timestamp) ->
	NextKey = ets:next(Tab,Key),
	CurrentTimestamp = insertion_timestamp(Tab,Key),
	if
		CurrentTimestamp < Timestamp ->
			ets:delete(Tab,Key);
		true -> ok
	end,
	delete_old(Tab, NextKey, Timestamp).
	

find_oldest_timestamp(_Tab,'$end_of_table',OldestTimestamp) -> OldestTimestamp;
find_oldest_timestamp(Tab,Key,OldestTimestamp) ->
	NextKey = ets:next(Tab,Key),
	CurrentTimestamp = insertion_timestamp(Tab, Key),
	if
		CurrentTimestamp < OldestTimestamp -> 
			find_oldest_timestamp(Tab, NextKey, CurrentTimestamp);
		true ->
			find_oldest_timestamp(Tab, NextKey, OldestTimestamp)
	end.
	
	
			
		
	
