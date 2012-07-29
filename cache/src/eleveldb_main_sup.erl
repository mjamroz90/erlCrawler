%% @doc Glowny supervisor dla bazy opartej na leveldb. Uruchamia on supervisor nizszego poziomu - eleveldb_disk_sup, oraz supervisor - eleveld_worker_sup. 

-module(eleveldb_main_sup).
-behaviour(supervisor).
-export([start/0]).
-export([init/1]).

start() ->	
	supervisor:start_link({local,?MODULE},?MODULE,[]).

%% @private
init([]) ->
	DiskSup = {eleveldb_disk_sup,{eleveldb_disk_sup,start,[]},
				permanent,brutal_kill,supervisor,[eleveldb_disk_sup]},	
	WorkerSup = {eleveldb_worker_sup,{eleveldb_worker_sup,start,[]},
				permanent,brutal_kill,supervisor,[eleveldb_worker_sup]},	
	
	RestartStrategy = {one_for_one,100,1},
	{ok,{RestartStrategy,[WorkerSup,DiskSup]}}.


