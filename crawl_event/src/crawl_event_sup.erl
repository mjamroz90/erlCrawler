%% @doc Supervisor nadzorujacy prace Event Manager'a
%% @end

-module(crawl_event_sup).
-behaviour(supervisor).
-export([start/0,init/1]).

start() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).
	
init([]) ->
	CrawlEvent = {crawl_event,{crawl_event,start,[]},
				permanent,brutal_kill,worker,[crawl_event]},
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[CrawlEvent]}}.
