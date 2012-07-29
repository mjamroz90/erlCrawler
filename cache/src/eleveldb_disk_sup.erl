%% @doc Supervisor uruchamiajacy proces eleveldb_disk_cache_server - serwer bazy url-i na dysku, oraz proces visited_urls_server - serwer bazy nieprzetworzonych url-i.

-module(eleveldb_disk_sup).
-behaviour(supervisor).
-export([start/0]).
-export([init/1]).

start() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

%% @private	
init([]) ->
	
	DiskServer = {eleveldb_disk_cache_server,{eleveldb_disk_cache_server,start,[]},
			  permanent,brutal_kill,worker,[eleveldb_disk_cache_server]},
	
	VisitedUrlServer = {visited_urls_server,{visited_urls_server,start,[]},
			  permanent,brutal_kill,worker,[visited_urls_server]},
			  		  
	RestartStrategy = {one_for_one,100,1},
	{ok,{RestartStrategy,[DiskServer,VisitedUrlServer]}}.


