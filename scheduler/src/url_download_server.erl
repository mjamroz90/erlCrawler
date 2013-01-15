-module(url_download_server).
-behaviour(gen_server).
-export([start/1, stop/0, pull/1, report/5, get_page_to_process/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state,{queue_limit, download_queue, processing_queue, processing_queue_size}).

%======================================API==================================

%% @spec start(QueueLimit :: integer()) -> {ok, Pid} | {error,term()}
%% @doc Uruchamia serwer dostarczajacy adresy do pobrania i przetwarzania.
%% Queue limit - rozmiar wewnetrznych kolejek.
%% @end
start(QueueLimit) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [QueueLimit], []).
	
%% @spec stop() -> ok
%% @doc Zatrzymuje serwer.
%% @end
stop() ->
	gen_server:cast(?MODULE,stop).

%% @spec pull(Count :: integer()) -> [{UrlId :: term(), Url :: string()}]
%% @doc Zwraca liste maksylanie Count adresow do pobrania
%% @end
pull(Count) ->
	gen_server:call(?MODULE, {pull, Count}).
	
%% @spec report(UrlId :: term(), Url :: string(), RedirectedUrl :: string(), Source :: binary(), Status :: ok/error)
%% @doc Przyjmuje informacje o przetworzeniu adresu, wraz z jego zrodlem lub informacja o bledzie, wartoscia RedirectedUrl powinien byc adres faktycznie pobranej strony
%% @end
report(UrlId, Url, RedirectedUrl, Source, Status) ->
	gen_server:cast(?MODULE, {report, UrlId, Url, RedirectedUrl, Source, Status}).
	
%% @spec get_page_to_process() -> empty/{UrlId :: term(), Url :: string(), RedirectedUrl :: string(), Source :: binary()}
%% @doc Zwraca identyfikator, adres i zrodlo strony do przetworzenia.
%% @end
get_page_to_process() ->
	gen_server:call(?MODULE, get_page_to_process).
	
	
%===================================Callbacks==============================	

%% @private
init([QueueLimit]) ->	
	State = #state{queue_limit = QueueLimit, download_queue=[], processing_queue=[], processing_queue_size=0},
	{ok, State}.
	
%% @private
handle_call({pull, Count}, _From,State = #state{queue_limit = QueueLimit,
processing_queue_size = ProcessingQueueSize}) when ProcessingQueueSize < QueueLimit ->
	DownloadQueue = State#state.download_queue,
	ReturnSize = if
		Count > QueueLimit - ProcessingQueueSize ->
			QueueLimit - ProcessingQueueSize;
		true ->
			Count
	end,
	%io:format("RETSIZE: ~p~n", [ReturnSize]),
	SplitPosition = if
		length(DownloadQueue) < ReturnSize ->
			length(DownloadQueue);
		true ->
			ReturnSize
	end,
	%io:format("POS: ~p~n", [SplitPosition]),
	{ReturnList, Rest} = lists:split(SplitPosition, DownloadQueue),
	%io:format("RetList: ~p, Rest: ~p ~n", [ReturnList, Rest]),
	
	NewDownloadQueue = if
		length(Rest) < QueueLimit/4 ->
			Rest ++ pull_urls(QueueLimit);
		true ->
			Rest
	end,
			
	{reply, ReturnList, State#state{download_queue = NewDownloadQueue}};
	
handle_call({pull, _Count}, _From, State) ->
	{reply, [], State};
	
handle_call(get_page_to_process, _From,
State = #state{processing_queue = ProcessingQueue, processing_queue_size = ProcessingQueueSize}) ->
	{Page, NewState} = case ProcessingQueueSize of
		0 ->
			{empty, State};
		_N ->
			[H | T] = ProcessingQueue,
			{H, State#state{processing_queue = T,
			processing_queue_size = ProcessingQueueSize - 1}}
	end,
	{reply, Page, NewState}.
	
	
	
%% @private
handle_cast({report, UrlId, Url, RedirectedUrl, Source, Status},
State = #state{processing_queue = ProcessingQueue, processing_queue_size = ProcessingQueueSize}) ->
	%io:format("got report ~p ~p~n", [Url, Status]),
	NewState = case Status of
		ok ->
			State#state{processing_queue = ProcessingQueue ++ [{UrlId, Url, RedirectedUrl, Source}],
			processing_queue_size = ProcessingQueueSize+1};
		error ->
			State
	end,
	{noreply, NewState};
			
handle_cast(stop,State) ->
	{stop,"Made to stop",State}.
	
%% @private	
handle_info(_Msg,State) ->
	{noreply,State}.

%% @private	
terminate(_Reason,_State) ->
	ok.

%% @private	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
	
%% @private	
pull_urls(Size) ->
	%[{1, "addr"}, {2, "addr"}].
	disk_cache_server:pull_urls(Size).
