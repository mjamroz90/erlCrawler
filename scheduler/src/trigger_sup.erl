-module(trigger_sup).
-behaviour(supervisor).
-export([start/1]).
-export([init/1]).

start(SleepTime) ->
	supervisor:start_link({local,?MODULE},?MODULE,[SleepTime]).
	
init([SleepTime]) ->	
	Trigger = {trigger,{trigger,start_link,[SleepTime]},
				permanent,brutal_kill,worker,[trigger]},
					
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[Trigger]}}.

