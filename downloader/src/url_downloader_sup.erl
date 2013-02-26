-module(url_downloader_sup).
-behaviour(supervisor).
-export([start/3]).
-export([init/1]).

%% ========================== API =============================

start(ConnTimeout,IdleTimeout,MaxWorkerCount) ->
	supervisor:start_link({local,?MODULE},?MODULE,[ConnTimeout,IdleTimeout,MaxWorkerCount]).

%% ======================== Callbacks =========================	

init([ConnTimeout,IdleTimeout,MaxWorkerCount]) ->
	PidStorageServer = {pid_storage_server,{pid_storage_server,start,[]},
				permanent,brutal_kill,worker,[pid_storage_server]},	
	UrlDownloader = {url_downloader,{url_downloader,start,[ConnTimeout,IdleTimeout,MaxWorkerCount]},
				permanent,brutal_kill,worker,[url_downloader]},

	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[PidStorageServer,UrlDownloader]}}.

