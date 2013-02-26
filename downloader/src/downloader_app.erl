-module(downloader_app).
-behaviour(application).
-export([start/2, stop/1]).

-define(CONN_TIMEOUT,3000).
-define(IDLE_TIMEOUT,5000).
-define(MAX_WORKERS_NUM,50).

start(_StartType,_StartArgs) ->
	ConnTimeout = case application:get_env(downloader,conn_timeout) of 
		undefined -> ?CONN_TIMEOUT;
		{ok,Value} -> Value 
	end,
	IdleTimeout = case application:get_env(downloader,idle_timeout) of 
		undefined -> ?IDLE_TIMEOUT;
		{ok,Value1} -> Value1 
	end,		
	MaxWorkersNum = case application:get_env(downloader,max_workers_num) of 
		undefined -> ?MAX_WORKERS_NUM;
		{ok,Value2} -> Value2
	end,
	inets:start(),
	url_downloader_sup:start(ConnTimeout,IdleTimeout,MaxWorkersNum).

stop(_State) ->
	ok.
	