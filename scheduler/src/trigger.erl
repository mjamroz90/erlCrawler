-module(trigger).
-export([start_link/1,trigger_function/1]).
         
start_link(SleepTime) ->
	Pid = spawn_link(trigger, trigger_function, [SleepTime]),
	{ok, Pid}.

trigger_function(SleepTime) ->
	timer:sleep(SleepTime),
	scheduler:completed(),
	trigger_function(SleepTime).
