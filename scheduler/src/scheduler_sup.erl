-module(scheduler_sup).
-behaviour(supervisor).
-export([start/2]).
-export([init/1]).

start(MaxProcessCount,BufferSize) ->
	supervisor:start_link({local,?MODULE},?MODULE,[MaxProcessCount,BufferSize]).
	
init([MaxProcessCount,BufferSize]) ->	
	SchedulerServer = {scheduler,{scheduler,start_link,[MaxProcessCount, BufferSize]},
				permanent,brutal_kill,worker,[scheduler]},
					
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[SchedulerServer]}}.

