%% @doc Modul uruchamiajacy cala aplikacje obslugujaca zdarzenia.
%% @end

-module(crawl_event_app).
-behaviour(application).
-export([start/2,stop/1]).

start(_StartType,_StartArgs) ->	
	crawl_event_sup:start().

stop(_State) ->
	ok.
