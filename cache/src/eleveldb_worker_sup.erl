%% @doc Supervisor uruchamiajacy i nadzorujacy procesy eleveldb_worker. Uruchamia je na zadanie eleveldb_disk_cache_server.

-module(eleveldb_worker_sup).
-behaviour(supervisor).
-export([start/0,start_worker/2]).
-export([init/1]).

start() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

%% @spec start_worker(UrlDbName :: string(), IdDbName :: string()) -> {ok,Pid :: pid()} | {error, term()}
%% @doc Uruchamia proces worker-a (eleveldb_worker) podajac mu nazwy baz.	
start_worker(UrlDbName,IdDbName)->
	supervisor:start_child(?MODULE,[UrlDbName,IdDbName]).

%% @private	
init([]) ->
	
	Worker = {eleveldb_worker,{eleveldb_worker,start,[]},
			  temporary,brutal_kill,worker,[eleveldb_worker]},
			  
	RestartStrategy = {simple_one_for_one,0,1},
	{ok,{RestartStrategy,[Worker]}}.


