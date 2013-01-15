-module(mock_event_handler).
-compile(export_all).
-behaviour(gen_event).
-record(state,{pid=undefined}).
-export([init/1,handle_event/2,code_change/3,terminate/2,handle_info/2,handle_call/2]).
 
init([]) ->
	{ok,#state{pid=undefined}};

init([Pid]) ->
	{ok,#state{pid=Pid}}.

handle_event({report_load,{_Node,Load}},State = #state{pid=Pid}) ->
	Pid ! {report_load,Load},
	{ok,State};

handle_event({log_message,Msg},State = #state{pid=Pid}) ->
	Pid ! {load_message,Msg},
	{ok,State};
	
handle_event({report_stats,Msg,_Test},State = #state{pid=Pid}) ->
	Pid ! {report_stats,Msg},
	{ok,State}.
	
handle_call(_Req,State) ->
	{ok,State}.

handle_info(_Info,State) ->
	{ok,State}.

terminate(_Args,_State) ->	
	ok.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
