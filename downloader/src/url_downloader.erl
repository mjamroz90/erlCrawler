-module(url_downloader).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start/3, download_content/1,stop/0]).
-record(state,{connection_timeout,idle_timeout,max_worker_count}).

start(ConnTimeout,IdleTimeout,MaxWorkerCount) ->
	gen_server:start_link({local,?MODULE},?MODULE,[ConnTimeout,IdleTimeout,MaxWorkerCount],[]).

%% zwraca {download_error,Url} | {Url,{PlainText,[link()]}}
download_content(Url) ->
	gen_server:call(?MODULE,{download_content,Url},infinity).

stop() ->
	gen_server:cast(?MODULE,stop).	

%% ========================= Callbacks ================================

init([ConnTimeout,IdleTime,MaxWorkerCount]) ->		
	{ok,#state{connection_timeout=ConnTimeout,idle_timeout=IdleTime,max_worker_count=MaxWorkerCount}}.

handle_cast(stop,State) ->
	{stop,"Made to stop",State}.

handle_call({download_content,Url},From,State = 
		#state{connection_timeout=ConnTimeout,idle_timeout=IdleTime,max_worker_count=MaxWorkerCount}) ->
	Pid = choose_worker(ConnTimeout,IdleTime,MaxWorkerCount),
	pid_storage_server:inc_mb_counter(Pid),
	download_worker:download_content(Pid,Url,From),
	{noreply,State}.

terminate(_Reason, _State) ->
	ok.

handle_info(_Msg,State) ->
	{noreply,State}.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

%% ========================= Private ===================================

choose_worker(ConnTimeout,IdleTime,MaxWorkerCount) ->
	case pid_storage_server:get_size() < MaxWorkerCount of 
		true -> 
			{ok,WorkerPid} = download_worker:start(ConnTimeout,IdleTime),
			pid_storage_server:insert(WorkerPid),
			WorkerPid;
		false ->
			pid_storage_server:fetch_pid()
	end.	
