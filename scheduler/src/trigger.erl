%% @doc Modul zajmujacy sie okresowym wzbudzaniem zarzadcy przetwarzania (schedulera)
%% @end

-module(trigger).
-export([start_link/1,trigger_function/1]).

%% @spec start_link(SleepTime) -> {ok, pid()}
%% @doc Uruchamia proces wzbudzajacy zarzadce co czas SleepTime (w milisekundach).
%% @end
start_link(SleepTime) ->
	Pid = spawn_link(trigger, trigger_function, [SleepTime]),
	{ok, Pid}.

%% @private
trigger_function(SleepTime) ->
	timer:sleep(SleepTime),
	scheduler:completed(),
	trigger_function(SleepTime).
