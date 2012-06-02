-module(processing_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/2, count_children/0, init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
start_child(Id, Url) ->
	supervisor:start_child(?MODULE, [Id, Url]).
	
count_children() ->
	supervisor:count_children(?MODULE).
	
init([]) ->
	Element = {url_processing, {url_processing, start_link, []},
		temporary, brutal_kill, worker, [url_processing]},
	
	RestartStrategy = {simple_one_for_one, 0, 10},
	
	{ok, {RestartStrategy, [Element]}}.
	
