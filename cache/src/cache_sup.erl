-module(cache_sup).
-behaviour(supervisor).
-export([start/3]).
-export([init/1]).

start(MaxItemNum,NodeName,Port) ->
	supervisor:start_link({local,cache_sup},?MODULE,[MaxItemNum,NodeName,Port]).
	
init([MaxItemNum,NodeName,Port]) ->
	RamCacheServer = {ram_cache_server,{ram_cache_server,start,[MaxItemNum]},
				permanent,brutal_kill,worker,[ram_cache_server]},	
	DiskCacheServer = {disk_cache_server,{disk_cache_server,start,[NodeName,Port]},
				permanent,brutal_kill,worker,[disk_cache_server]},
	
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[RamCacheServer,DiskCacheServer]}}.

