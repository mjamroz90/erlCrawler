-module(scheduler_app).
-behaviour(application).

-export([start/2,stop/1]).

-define(MAX_PROCESS_COUNT,1000000).
-define(BUFFER_SIZE,10000).

start(_StartType,_StartArgs) ->	
	MaxProcessCount = case application:get_env(erlCrawler,max_process_count) of
			{ok,M} -> M;
			undefined -> ?MAX_PROCESS_COUNT
		end,
	BufferSize = case application:get_env(erlCrawler,buffer_size) of
			{ok,N} -> N;
			undefined -> ?BUFFER_SIZE
		end,

	scheduler_sup:start(MaxProcessCount,BufferSize),
	processing_sup:start_link().

stop(_State) ->
	scheduler:stop(),
	ok.
