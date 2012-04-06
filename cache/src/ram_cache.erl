-module(ram_cache).
-export([new/1,insert/3,delete/2,lookup/2,test/0,exists/2,remove_random/1,remove/1,size/1]).

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
	
insert(Tab,Url,Value) ->
	ets:insert(Tab,{Url,Value}).
	
delete(Tab,Url) ->
	ets:delete(Tab,Url).
	
lookup(Tab,Url) ->	
	case ets:lookup(Tab,Url) of
		[] -> [];
		[{_,Value}] -> Value
	end.	

exists(Tab,Url) ->
	case lookup(Tab,Url) of 
		[] -> false;
		_ -> true
	end.

remove_random(Tab) ->
	remove_random(Tab,1).

size(Tab) ->
	ets:info(Tab,size).
	
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
	