-module(domain_manager_sup).
-behaviour(supervisor).
-export([start/2]).
-export([init/1]).

start(TargetFun,Nodes) ->
	supervisor:start_link({local,?MODULE},?MODULE,[TargetFun,Nodes]).
	
init([TargetFun,Nodes]) ->	
	DomainDispServer = {domain_dispatch_server,{domain_dispatch_server,start,[Nodes]},
				permanent,brutal_kill,worker,[domain_dispatch_server]},
				
	LoadCollectorServer = {load_collector_server,{load_collector_server,start,[TargetFun,Nodes]},
				permanent,brutal_kill,worker,[load_collector_server]},
					
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[DomainDispServer,LoadCollectorServer]}}.

