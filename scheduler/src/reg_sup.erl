%% @doc Supervisor nadzorujacy serwer wyrazen regularnych.
-module(reg_sup).
-behaviour(supervisor).

-export([start/0, init/1]).

%% @spec start() -> {ok, pid()} | {error, term()}
start() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).
	
%% @private
init([]) ->
	RegServer = {reg, {reg, start_link, []},
				permanent, brutal_kill, worker, [reg]},
	
	RestartStrategy = {one_for_all,10,1},
	{ok,{RestartStrategy,[RegServer]}}.
