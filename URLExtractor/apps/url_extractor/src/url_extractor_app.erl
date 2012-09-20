-module(url_extractor_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, extract/2]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

extract(ID,URL)->
	url_extractor:extract(ID,URL).

start(_StartType, _StartArgs) ->
	url_extractor_sup:start_link().

stop(_State) ->
    ok.

