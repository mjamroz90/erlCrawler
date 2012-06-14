-module(reg_sup).
-behaviour(supervisor).

-export([start/0, init/1]).

start() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).
	
init([]) ->
	RegServer = {reg, {reg, start_link, []},
				permanent, brutal_kill, worker, [reg]},
	
	RestartStrategy = {one_for_all,10,1},
	{ok,{RestartStrategy,[RegServer]}}.
