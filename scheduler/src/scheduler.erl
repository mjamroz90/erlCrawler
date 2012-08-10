-module(scheduler).
-behaviour(gen_server).
-export([start_link/2,insert/2,completed/0, stop/0]).
-record(state,{process_limit, buffer_size, urls, current_process_count}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
    
-define(URL_BUCKET,term_to_binary("Urls")).
    
%======================================API==================================
         
start_link(ProcessLimit, BufferSize) ->
	gen_server:start_link({local,?MODULE},?MODULE,[ProcessLimit, BufferSize],[]).
	
insert(Url, Params) ->
	gen_server:call(?MODULE,{insert,{Url, Params}}).
	
completed() ->
	gen_server:cast(?MODULE,completed).
		
stop() ->
	gen_server:cast(?MODULE,stop).
	
%===================================Callbacks==============================

init([ProcessLimit, BufferSize]) ->	
	State = #state{process_limit = ProcessLimit, buffer_size = BufferSize, urls = [], current_process_count = 0},
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
	
	NewState = State#state{current_process_count = current_process_count(processing_sup:count_children()), urls = Urls ++ NewUrls},
	gen_server:cast(?MODULE, process_next),
	{noreply, NewState};
	
handle_cast(process_next, State) when State#state.process_limit /= State#state.current_process_count ->
	
	case State#state.urls of	
		[H | T] -> 
			processing_sup:start_child(common:get_param(id, url_server:lookup(H)),H),
			
			NewState = State#state{current_process_count = current_process_count(processing_sup:count_children()), urls = T},
			gen_server:cast(?MODULE, process_next);
			
		[] ->
			NewState = State#state{current_process_count = current_process_count(processing_sup:count_children())} 
			
			%TODO tu trzeba podpiac proces, ktory bedzie co jakis czas sprawdzal czy nie ma czasem juz czegos w bazie
	end,
		
	{noreply, NewState};

handle_cast(process_next, State) ->
	{noreply, State}.
	
handle_call({insert,{Url, Params}}, _From, State) ->	
	case url_server:lookup(Url) of
		not_found ->
			url_server:insert(Url, Params),
			Id = common:get_param(id, url_server:lookup(Url)),
			visited_urls_server:insert(Id);
		ExistingParams ->
			process_new_params(Url, ExistingParams, Params)
	end,
	{reply,ok,State}.
	
	
pull_urls() ->
	%Urls = disk_cache_server:pull_urls(100, false),
	%Urls = disk_cache_server:pull_urls(100),
	Urls = visited_urls_server:pull_urls(100), % due to a bug in disk_cache_server (gen_server call loop)
	%set_visited(Urls),
	Urls.
	
%set_visited([]) -> ok;
%set_visited([H|T]) ->
	%disk_cache_server:set_visited(H),
	%set_visited(T).
	
current_process_count(PropListOfCounts) ->
	[_, {active, Count}, _, _] = PropListOfCounts,
	Count.
	
process_new_params(Url, OldParams, NewParams) ->
	OldTimestamp = common:get_param(timestamp, OldParams),
	OldWidth = common:get_param(width, OldParams),
	OldDepth = common:get_param(depth, OldParams),
	
	Width = common:get_param(width, NewParams),
	Depth = common:get_param(depth, NewParams),
		
	if
		OldWidth > Width -> %bylismy blizej
			true;
		((Width > OldWidth) and (Depth >= 0)) or ((Width == OldWidth) and (Depth > OldDepth)) ->
			%jestesmy w mniejszej szerokosci badz w tej samej, ale na mniejszej glebokosci
			%Params = [{timestamp, OldTimestamp}, {width, Width}, {depth, Depth}],
			Params = common:stick_params({timestamp, OldTimestamp},
				common:stick_params({width, Width},
					common:stick_params({depth, Depth}, OldParams)
				)
			),
			url_server:update(Url, Params);
		true ->
			false
	end,

	%sprawdzamy timestamp czy czas juz odswiezyc
	ValidityTime = session_manager:get_validity_time(reg:get_domain(Url)),
	RefreshTime = OldTimestamp + ValidityTime,
	CurrentTime = common:timestamp(),
	if
		CurrentTime > RefreshTime ->
			%disk_cache_server:set_not_visited(Url);
			io:format("refreshing ~p ~p ~p ~n", [Url, RefreshTime, CurrentTime]),
			visited_urls_server:insert(common:get_param(id, OldParams));
		true ->
			false
	end.
		
	
handle_info(_Msg,State) ->
	{noreply,State}.
	
terminate(_Reason,_State) ->
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	

