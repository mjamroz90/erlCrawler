-module(url_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
		 
-export([start/0,insert/2,lookup/1, update/2,stop/0]).

%================================API===========================================
start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).
	
insert(Url,Params) ->
	gen_server:call(?MODULE,{insert,{Url,Params}}).

update(Url, Params) ->
	gen_server:call(?MODULE,{update, {Url, Params}}).

lookup(Url) ->
	gen_server:call(?MODULE,{lookup,Url}).
	
stop() ->
	gen_server:cast(?MODULE,stop).
	
%===============================Callbacks======================================

init(State) ->
	{ok,State}.
	

handle_cast(stop,State) ->
	{stop,"Made to stop",State}.

handle_call({insert,{Url,Params}},_From, State) ->
	disk_cache_server:insert(Url, Params),
	ram_cache_server:insert(Url, Params),
	{reply,ok,State};
	
handle_call({update, {Url, Params}}, _From, State) ->
	disk_cache_server:update(Url, Params),
	ram_cache_server:delete(Url),
	ram_cache_server:insert(Url, Params),
	{reply,ok,State};

handle_call({lookup,Url},_From,State) ->
	Result = case ram_cache_server:lookup(Url) of
		not_found -> 
			%not found in memory, search in db
			case disk_cache_server:lookup(Url) of
				not_found -> not_found;
				Value ->
					%found, copy to ram
					ram_cache_server:insert(Url, Value),
					Value
			end;
		Value -> Value
	end,
	{reply,Result,State}.
	
handle_info(_Msg,State) ->
	{noreply,State}.

terminate(_Reason,_State) ->
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
