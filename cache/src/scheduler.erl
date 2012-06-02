-module(scheduler).
-export([start_link/2,insert/2,completed/0, stop/0]).
-record(state,{process_count, buffer_size, urls, current_processes_count}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
    
-define(URL_BUCKET,term_to_binary("Urls")).
    
%======================================API==================================
         
start_link(ProcessCount, BufferSize) ->
	gen_server:start_link({local,?MODULE},?MODULE,[ProcessCount, BufferSize],[]).
	
insert(Url, Params) ->
	gen_server:call(?MODULE,{insert,{Url, Params}}).
	
completed() ->
	gen_server:cast(?MODULE,completed).
		
stop() ->
	gen_server:cast(?MODULE,stop).
	
%===================================Callbacks==============================

init([ProcessCount, BufferSize]) ->	
	State = #state{process_count = ProcessCount, buffer_size = BufferSize, urls = [], current_processes_count = 0},
	{ok, State}.
	
		
handle_cast(stop,State) ->
	{stop,"Made to stop",State};
	
handle_cast(completed, State) ->
	CurrentBufferSize = length(State#state.urls),
	MaxBufferSize = State#state.buffer_size,
	
	case CurrentBufferSize < MaxBufferSize/4 of
		true ->
			NewUrls = pull_urls();
		false ->
			NewUrls = []
	end,
	
	Urls = State#state.urls,
	
	NewState = State#state{process_count = processing_sup:count_children(), urls = Urls ++ NewUrls},
	gen_server:cast(?MODULE, process_next),
	{noreply, NewState};
	
handle_cast(process_next, State) when State#state.process_count /= State#state.current_processes_count ->
	io:format("next_url"),
	case State#state.urls of	
		[H | T] -> 
			processing_sup:start_child(124,H),
	
			NewState = State#state{process_count = processing_sup:count_children(), urls = T},
			gen_server:cast(?MODULE, process_next);
			
		[] ->
			NewState = State#state{process_count = processing_sup:count_children()} %z count children trzeba wyciagnac odpowiednie dane
			
			%TODO tu trzeba podpiac proces, ktory bedzie co jakis czas sprawdzal czy nie ma czasem juz czegos w bazie
	end,
		
	{noreply, NewState};

handle_cast(process_next, State) ->
	{noreply, State}.
	
handle_call(insert, _From, State) ->
		
	{reply,ok,State}.	
	
	
pull_urls() ->
	disk_cache_server:pull_urls(100).
	%TODO - zaznaczyc jako przekazane do przetwarzania
		
	
handle_info(_Msg,State) ->
	{noreply,State}.
	
terminate(_Reason,_State) ->
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
	
