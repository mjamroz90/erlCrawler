-module(load_collector_handler).
-behaviour(gen_event).
-export([init/1,handle_event/2,code_change/3,terminate/2,handle_info/2,handle_call/2]).
-define(LOAD_COLLECTOR_SERVER,load_collector_server).

init([NodeToReport]) ->
	{ok,NodeToReport}.
	
handle_event({report_load,{Node,Load}},NodeToReport) ->
	gen_server:cast({?LOAD_COLLECTOR_SERVER,NodeToReport},{report_load,{Node,Load}}),
	{ok,NodeToReport}.

handle_call(_Req,State) ->
	{ok,ok,State}.

handle_info(_Info,State) ->
	{ok,State}.
	
terminate(_Args,_State) ->	
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
