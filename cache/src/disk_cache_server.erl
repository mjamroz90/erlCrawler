-module(disk_cache_server).
-export([start/2,insert/2,lookup/1,update/2,delete/1,change_host/2,stop/0]).
-record(state,{nodename, port, riakc_pid}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
    
-define(URL_BUCKET,term_to_binary("Urls")).
    
%======================================API==================================
         
start(NodeName,Port) ->
	gen_server:start_link({local,?MODULE},?MODULE,[NodeName,Port],[]).
	
insert(Url,Params) ->
	gen_server:cast(?MODULE,{insert,{Url,Params}}).
	
lookup(Url) ->
	gen_server:call(?MODULE,{lookup,Url}).
	
update(Url,Params) ->
	gen_server:cast(?MODULE,{update,{Url,Params}}).
	
delete(Url) ->
	gen_server:cast(?MODULE,{delete,Url}).
	
change_host(NewNodeName,NewPort) ->
	gen_server:call(?MODULE,{change_host,{NewNodeName,NewPort}}).
	
stop() ->
	gen_server:cast(?MODULE,stop).
	
%===================================Callbacks==============================

init([NodeName,Port]) ->	
	case riakc_pb_socket:start_link(NodeName,Port) of
		{ok,Pid} -> {ok,#state{riakc_pid = Pid}};
		{error,Reason} -> {stop,Reason}
	end.	
	
handle_cast({insert,{Url,Params}},State = #state{riakc_pid = Pid}) ->
	Object = riakc_obj:new(?URL_BUCKET,term_to_binary(Url),term_to_binary(Params)),
	riakc_pb_socket:put(Pid,Object),
	{noreply,State};
	
handle_cast({update,{Url,Params}},State = #state{riakc_pid = Pid}) ->
	{ok,Obj} = riakc_pb_socket:get(Pid,?URL_BUCKET,term_to_binary(Url)),
	Updated_Obj = riakc_obj:update_value(Obj,term_to_binary(Params)),
	riakc_pb_socket:put(Pid,Updated_Obj),
	{noreply,State};
	
handle_cast({delete,Url},State = #state{riakc_pid = Pid}) ->
	riakc_pb_socket:delete(Pid,?URL_BUCKET,term_to_binary(Url)),
	{noreply,State};
	
handle_cast(stop,State) ->
	{stop,"Made to stop",State}.
	
handle_call({lookup,Url},_From,State = #state{riakc_pid = Pid}) ->
	Result = case riakc_pb_socket:get(Pid,?URL_BUCKET,term_to_binary(Url)) of
		{ok,Object} -> 
			binary_to_term(riakc_obj:get_value(Object));
			
		{error,_} -> not_found
	end,
	{reply,Result,State};	
 
handle_call({change_host,{NewNodeName,NewPort}},_From,State = #state{riakc_pid=Pid}) ->
	Result = case riakc_pb_socket:start_link(NewNodeName,NewPort) of
		{ok,NewPid} -> ok;
		
		{error,Reason} -> NewPid = Pid,{error,Reason}
	end,
	{reply,Result,State#state{nodename=NewNodeName,port=NewPort,riakc_pid=NewPid}}.
	
handle_info(_Msg,State) ->
	{noreply,State}.
	
terminate(_Reason,#state{riakc_pid = Pid}) ->
	riakc_pb_socket:stop(Pid),
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
	
