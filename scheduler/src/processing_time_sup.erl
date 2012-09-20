-module(processing_time_sup).
-behaviour(supervisor).
-export([start/0]).
-export([init/1]).

start() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).


%% @private
init([]) ->	
	ProcessingTimeServer = {processing_time_server,{processing_time_server,start,[]},
				permanent,brutal_kill,worker,[processing_time_server]},
					
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[ProcessingTimeServer]}}.

