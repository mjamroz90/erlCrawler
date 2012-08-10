-module(scheduler_app).
-behaviour(application).

-export([start/2,stop/1]).

-define(MAX_PROCESS_COUNT,5).
-define(BUFFER_SIZE,1000).
-define(TRIGGER_TIME, 3000).

start(_StartType,_StartArgs) ->	
	MaxProcessCount = case application:get_env(erlCrawler,max_process_count) of
			{ok,M} -> M;
			undefined -> ?MAX_PROCESS_COUNT
		end,
	BufferSize = case application:get_env(erlCrawler,buffer_size) of
			{ok,N} -> N;
			undefined -> ?BUFFER_SIZE
		end,
		
	TriggerTime = case application:get_env(erlCrawler,trigger_time) of
			{ok,Time} -> Time;
			undefined -> ?TRIGGER_TIME
		end,

	reg_sup:start(),
	processing_sup:start_link(),
	trigger_sup:start(TriggerTime),
	scheduler_sup:start(MaxProcessCount,BufferSize).

stop(_State) ->
	scheduler:stop(),
	reg_sup:stop(),
	ok.
