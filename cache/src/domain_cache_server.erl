-module(domain_cache_server).
-export([start/0,lookup/1,stop/0]).
-record(state,{}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
    
-define(TABLE,domains).
    
%======================================API==================================
         
start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).
	
lookup(Url) ->
	gen_server:call(?MODULE,{lookup,Url}).
	
% update(Url,Params) ->
	% gen_server:call(?MODULE,{update,{Url,Params}}).
	
% delete(Url) ->
	% gen_server:call(?MODULE,{delete,Url}).

% delete_all() ->
	% gen_server:call(?MODULE,delete_all,infinity).
	
% change_host(NewNodeName,NewPort) ->
	% gen_server:call(?MODULE,{change_host,{NewNodeName,NewPort}}).
	
stop() ->
	gen_server:cast(?MODULE,stop).
	
%===================================Callbacks==============================

init([]) ->	
	% case riakc_pb_socket:start_link(NodeName,Port) of
		% {ok,Pid} -> {ok,#state{riakc_pid = Pid}};
		% {error,Reason} -> {stop,Reason}
	% end.
	{ok,#state{}}.
	
handle_cast(stop,State) ->
	{stop,"Made to stop",State}.
	
% handle_call(delete_all,_From,State = #state{riakc_pid = Pid}) ->
	% io:format("Szukam kluczy\n"),
	% Result = case riakc_pb_socket:list_keys(Pid,?URL_BUCKET) of		
		% {ok,Keys} -> 			
			% io:format("Znalazlem klucze\n,dlugosc listy:~p\n",[length(Keys)]),lists:foreach(fun(Key) -> riakc_pb_socket:delete(Pid,?URL_BUCKET,Key) end,Keys),ok;
		% Err ->	Err
	% end,	
	% {reply,Result,State};	
		
% handle_call({update,{Url,Params}},_From,State = #state{riakc_pid = Pid}) ->
	% {ok,Obj} = riakc_pb_socket:get(Pid,?URL_BUCKET,term_to_binary(Url)),
	% Updated_Obj = riakc_obj:update_value(Obj,term_to_binary(Params)),
	% Result = riakc_pb_socket:put(Pid,Updated_Obj),
	% {reply,Result,State};
	
% handle_call({insert,{Url,Params}},_From,State = #state{riakc_pid = Pid}) ->	
	% Object = riakc_obj:new(?URL_BUCKET,term_to_binary(Url),term_to_binary(Params)),
	% Result = riakc_pb_socket:put(Pid,Object),
	% {reply,Result,State};
	
%===Patrzymy do Cache, pozniej do mnesii		
handle_call({lookup,Url},_From,State) ->
	Result = case domain_ram_cache_server:lookup(Url) of 
		not_found ->
			F = fun() -> mnesia:read(domain_dispatch_server:get_domain_table_name(),Url) end,
			{atomic,Result1} = mnesia:transaction(F),
			case Result1 of
				[{domain_to_node,Url,Node}] -> Node;
				[] -> not_found
			end;	
		Node -> Node
	end,	
	{reply,Result,State}.
	 
	
% handle_call({delete,Url},_From,State = #state{riakc_pid = Pid}) ->
	% Result = riakc_pb_socket:delete(Pid,?URL_BUCKET,term_to_binary(Url)),
	% {reply,Result,State}; 

	 
% handle_call({change_host,{NewNodeName,NewPort}},_From,State = #state{riakc_pid=Pid}) ->
	% Result = case riakc_pb_socket:start_link(NewNodeName,NewPort) of
		% {ok,NewPid} -> riakc_pb_socket:stop(Pid),ok;
		
		% {error,Reason} -> NewPid = Pid,{error,Reason}
	% end,
	% {reply,Result,State#state{nodename=NewNodeName,port=NewPort,riakc_pid=NewPid}}.
	
handle_info(_Msg,State) ->
	{noreply,State}.
	
terminate(_Reason,_State) ->
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
	
