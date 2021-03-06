%% @doc Modul uruchamiajacy aplikacje sterujaca przetwarzaniem.
%% @end
-module(scheduler_app).
-behaviour(application).

-export([start/2,stop/1]).

-define(MAX_PROCESS_COUNT,5).
-define(BUFFER_SIZE,1000).
-define(TRIGGER_TIME, 3000).
-define(STATS_PART_SIZE, 2000).
-define(STATS_FILENAME, "stats").

%% @private
start(_StartType,_StartArgs) ->	
	MaxProcessCount = case application:get_env(session_manager,max_process_count) of
			{ok,M} -> M;
			undefined -> ?MAX_PROCESS_COUNT
		end,
	BufferSize = case application:get_env(session_manager,buffer_size) of
			{ok,N} -> N;
			undefined -> ?BUFFER_SIZE
		end,
		
	StatsPartSize = case application:get_env(session_manager,stats_part_size) of
			{ok,S} -> S;
			undefined -> ?STATS_PART_SIZE
		end,
		
	StatsFile = case application:get_env(session_manager,stats_file) of
			{ok,Filename} -> Filename;
			undefined -> ?STATS_FILENAME
		end,

  TriggerTime = ?TRIGGER_TIME,
%	TriggerTime = case application:get_env(session_manager,trigger_time) of
%			{ok,Time} -> Time;
%			undefined -> ?TRIGGER_TIME
%		end,
  mockparser:start("plik",false), % TODO - REMOVE IT!
    stats_sup:start(StatsFile, StatsPartSize),
	reg_sup:start(),
	processing_sup:start_link(),
	trigger_sup:start(TriggerTime),
	processing_time_sup:start(),
    crawl_event:log_message({info,node(),scheduler,start,prepareMsgContent(MaxProcessCount,BufferSize,TriggerTime)}),
	scheduler_sup:start(MaxProcessCount,BufferSize).

%% @private
stop(_State) ->
    MsgContent = "Aplikacja scheduler zostala zatrzymana\n",
    crawl_event:log_message({info,node(),scheduler,stop,MsgContent}),
	ok.

%% @private
prepareMsgContent(MaxProcessCount,BufferSize,TriggerTime) ->
    lists:flatten(io_lib:format("Aplikacja scheduler wystartowala z parametrami :\n
    - liczba procesow sciagajacych : ~p\n
    - wielkosc kolejki Url-i : ~p\n
    - interwal czasu odpytywania bazy : ~p\n
    ",[MaxProcessCount,BufferSize,TriggerTime])).
