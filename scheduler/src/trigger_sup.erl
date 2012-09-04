%% @doc Supervisor nadzorujacy prace procesu wzbudzajacego prace zarzadcy przetwarzania (schedulera).
%% @end

-module(trigger_sup).
-behaviour(supervisor).
-export([start/1]).
-export([init/1]).

%% @spec start(SleepTime) -> {ok, pid()} | {error, term()}
%% @doc Uruchamia nadzorcę dla procesu wzbudzajacego zarzadce przetwarzania (schedulera) z interwalem SleepTime (w milisekundach).
%% @end
start(SleepTime) ->
	supervisor:start_link({local,?MODULE},?MODULE,[SleepTime]).
	
%% @private
init([SleepTime]) ->	
	Trigger = {trigger,{trigger,start_link,[SleepTime]},
				permanent,brutal_kill,worker,[trigger]},
					
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[Trigger]}}.

