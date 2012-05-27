-module(domain_manager_sup).
-behaviour(supervisor).
-export([start/1]).
-export([init/1]).

start(Nodes) ->
	supervisor:start_link({local,?MODULE},?MODULE,[Nodes]).
	
init([Nodes]) ->	
	DomainDispServer = {domain_dispatch_server,{domain_dispatch_server,start,[Nodes]},
				permanent,brutal_kill,worker,[domain_dispatch_server]},
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[DomainDispServer]}}.

