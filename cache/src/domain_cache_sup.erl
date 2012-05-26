-module(domain_cache_sup).
-behaviour(supervisor).
-export([start/1]).
-export([init/1]).

start(MaxItemNum) ->
	supervisor:start_link({local,?MODULE},?MODULE,[MaxItemNum]).
	
init([MaxItemNum]) ->
	RamCacheDomainServer = {domain_ram_cache_server,{domain_ram_cache_server,start,[MaxItemNum]},
				permanent,brutal_kill,worker,[domain_ram_cache_server]},	
	DomainCacheServer = {domain_cache_server,{domain_cache_server,start,[]},
				permanent,brutal_kill,worker,[domain_cache_server]},
	
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[RamCacheDomainServer,DomainCacheServer]}}.