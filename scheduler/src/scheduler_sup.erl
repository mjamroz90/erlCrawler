%% @doc Supervisor nadzorujacy zarzadce procesu przetwarzania.
%% @end
-module(scheduler_sup).
-behaviour(supervisor).
-export([start/2]).
-export([init/1]).

%% @spec start(MaxProcessCount :: integer(), BufferSize :: integer()) -> {ok, pid()} | {error, term()}
%% @doc Uruchamia nadzorce dla zarzadcy procesu przetwarzania z parametrami: <dl>
%% <dt>MaxProcessCount</dt><dd>maksymalna liczba rownoczesnie aktywnych procesow przetwarzajacych</dd>
%% <dt>BufferSize</dt><dd>rozmiar podrecznego bufora adresow do przetwarzania</dd>
%% </dl>
%% @end
start(MaxProcessCount,BufferSize) ->
	supervisor:start_link({local,?MODULE},?MODULE,[MaxProcessCount,BufferSize]).


%% @private
init([MaxProcessCount,BufferSize]) ->	
	SchedulerServer = {scheduler,{scheduler,start_link,[MaxProcessCount, BufferSize]},
				permanent,brutal_kill,worker,[scheduler]},
				
	UrlDownloadServer = {url_download_server,{url_download_server,start,[BufferSize]},
				permanent,brutal_kill,worker,[url_download_server]},
					
	RestartStrategy = {one_for_one,100,1},
	{ok,{RestartStrategy,[SchedulerServer, UrlDownloadServer]}}.

