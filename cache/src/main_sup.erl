-module(main_sup).
-behaviour(supervisor).
-export([start/4]).
-export([init/1]).

start(MaxItemNumCache,MaxItemNumDomain,NodeName,Port) ->
	supervisor:start_link({local,?MODULE},?MODULE,[MaxItemNumCache,MaxItemNumDomain,NodeName,Port]).
	
init([MaxItemNumCache,MaxItemNumDomain,NodeName,Port]) ->
	CacheSup = {cache_sup,{cache_sup,start,[MaxItemNumCache,NodeName,Port]},
				permanent,brutal_kill,supervisor,[cache_sup]},	
	DomainCacheSup = {domain_cache_sup,{domain_cache_sup,start,[MaxItemNumDomain]},
				permanent,brutal_kill,supervisor,[domain_cache_sup]},
	
	RestartStrategy = {one_for_one,100,1},
	{ok,{RestartStrategy,[CacheSup,DomainCacheSup]}}.


