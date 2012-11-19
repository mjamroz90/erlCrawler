%% @doc Supervisor nadzorujacy serwer statystyk
%% @end
-module(stats_sup).
-behaviour(supervisor).
-export([start/2]).
-export([init/1]).

%% @spec start(LogToFile :: string()/false, PartSize :: int()) -> {ok, Pid} | {error,term()}
%% @doc Uruchamia nadzorce dla serwera statystyk z parametrami: <dl>
%% <dt>LogToFile</dt><dd>nazwa pliku zapisu statystyk lub false</dd>
%% <dt>PartSize</dt><dd>ilosc stron dla pomiaru chwilowego</dd>
%% </dl>
%% @end         
start(LogToFile,PartSize) ->
	supervisor:start_link({local,?MODULE},?MODULE,[LogToFile,PartSize]).


%% @private
init([LogToFile,PartSize]) ->	
	StatsServer = {stats,{stats,start,[LogToFile, PartSize]},
				permanent,brutal_kill,worker,[stats]},
					
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[StatsServer]}}.
