%% @doc Serwer, ktory stanowi interfejs dostepu do bazy Urli na dysku, ktora jest eleveldb.
%% @end

-module(eleveldb_disk_cache_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start/0,stop/0,insert/2,update/2,lookup/1,delete/1,pull_urls/1,get_url_by_id/1]).
-define(PARTITION_NUM,get_env_property(partition_num)).
-define(BIT_ADDRESS_SPACE,32).
-define(ROOT_DIR,get_env_property(root_dir)).
-record(state,{worker_list,slice,max_rem}).

%% @spec start() -> {ok,Pid :: pid()} | {error,term()}
%% @doc Startuje proces serwera odbierajacaego polecenia operacji na bazie od klientow z zewnatrz. 
%% Do bezposrednich operacji na bazie uzywa on konfigurowalna liczbe procesow tzw. - workerow, ktore uruchamia podczas startu. 
start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[?ROOT_DIR],[]).

%% @spec insert(Url :: key(), Params :: proplist()) -> 	{ok,NewParams :: proplist()} | {error, term()}
%% @doc Wstawia do bazy pare klucz-wartosc.
insert(Url,Params) ->
	gen_server:call(?MODULE,{insert,{Url,Params}}).

%% @spec update(Url :: key(), Params :: proplist()) -> {ok, NewParams :: proplist()} | not_found | {error, term()}
%% @doc Aktualizuje obiekt o podanym kluczu podana wartoscia.	
update(Url,Params) ->
	gen_server:call(?MODULE,{update,{Url,Params}}).

%% @spec delete(Url :: key()) -> ok | {error, term()}
%% @doc Usuwa obiekt o podanym kluczu.	
delete(Url) ->
	gen_server:call(?MODULE,{delete,Url}).

%% @spec lookup(Url :: key()) -> proplist() | not_found
%% @doc Wyszukuje w bazie obiekt o danym kluczu.
lookup(Url) ->
	gen_server:call(?MODULE,{lookup,Url}).

%% @spec pull_urls(Count :: integer()) -> [key()]
%% @doc Zwraca liste kluczy o dlugosci Count, ktore sa  nieprzetworzone.	
pull_urls(Count) ->
	gen_server:call(?MODULE,{pull_urls,Count}).

%% @spec get_url_by_id(Id :: integer()) -> key() | not_found
%% @doc Zwraca adres, ktory ma przypisany dany identyfikator.
get_url_by_id(Id) ->
	gen_server:call(?MODULE,{get_url_by_id,Id}).
	
%% @doc Zatrzymuje proces, wraz z procesami operujacymi na bazie.	
stop() ->
	gen_server:cast(?MODULE,stop).
	
%==================================Callbacks==================================

%% Utworz PARTIION_NUM procesow. Nastepnie trzeba utworzyc katalogi dla ich baz.

%% @private
init([RootDir]) ->
	MaxRem = (2 bsl (?BIT_ADDRESS_SPACE-1)),
	WorkerList = create_processes(MaxRem,?PARTITION_NUM,RootDir),
	{ok,#state{worker_list = WorkerList, slice = (MaxRem div ?PARTITION_NUM), max_rem = MaxRem}}.

%% @private	
handle_cast(stop,State) ->
	{stop, "Made to stop", State}.

%% @private	
handle_call({insert,{Url,Params}},From,State = #state{worker_list=Workers,slice = Slice, max_rem = MaxRem}) ->
	Indx = get_index(hash(Url),Slice,MaxRem),	
	{_,{pid,Pid}} = lists:nth(Indx,Workers),	
	eleveldb_worker:insert(Pid,From,Url,Params),
	{noreply,State};
	
handle_call({update,{Url,Params}},From,State = #state{worker_list=Workers,slice = Slice, max_rem = MaxRem}) ->
	Indx = get_index(hash(Url),Slice,MaxRem),
	{_,{pid,Pid}} = lists:nth(Indx,Workers),
	eleveldb_worker:update(Pid,From,Url,Params),
	{noreply,State};

handle_call({delete,Url},From,State = #state{worker_list=Workers,slice = Slice, max_rem = MaxRem}) ->
	Indx = get_index(hash(Url),Slice,MaxRem),
	{_,{pid,Pid}} = lists:nth(Indx,Workers),
	eleveldb_worker:delete(Pid,From,Url),
	{noreply,State};
	
handle_call({pull_urls,Count},_From,State) ->
	{reply,visited_urls_server:pull_urls(Count),State};

handle_call({get_url_by_id,Id}, _From, State = #state{worker_list=Workers}) ->
	{reply,lookup_id(Id,Workers),State};
	
handle_call({lookup,Url},From,State = #state{worker_list=Workers,slice = Slice, max_rem = MaxRem}) ->
	Indx = get_index(hash(Url),Slice,MaxRem),
	{_,{pid,Pid}} = lists:nth(Indx,Workers),
	eleveldb_worker:lookup(Pid,From,Url),
	{noreply,State}.

%% @private			
handle_info(_Msg,State) ->
	{noreply,State}.

%% @private
terminate(_Reason, #state{worker_list = Workers}) ->	
	lists:foreach(fun({_,{_,Pid}}) -> gen_server:cast(Pid,stop) end,Workers),
	ok.

%% @private
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

 
%=====================================Internal=================================

lookup_id(_Id,[]) ->
	not_found;

lookup_id(Id,[{_,{pid,Pid}} | T]) ->
	case eleveldb_worker:get_url_by_id(Pid,Id) of
		not_found -> lookup_id(Id,T);
		Url -> Url
	end.	

hash(Key) -> 
	erlang:phash2(Key, 2 bsl (?BIT_ADDRESS_SPACE -1)).

get_index(HashValue,Slice,MaxRem) ->
	HashMod = HashValue rem MaxRem,
	(HashMod div Slice)+1.
	
create_processes(MaxRem,PartitionNum,RootDir) ->
	Slice = MaxRem div PartitionNum,
	Workers = lists:map(fun(Num) -> create_process(Slice*(Num-1),RootDir) end,lists:seq(1,PartitionNum)),
	Workers.	
	
create_process(DownBound,RootDir) ->
	ProcessRootDir = filename:join(RootDir,lists:flatten(io_lib:format("~p", [DownBound]))),
	UrlDbName = filename:join(ProcessRootDir,"UrlDb"),
	IdDbName = filename:join(ProcessRootDir,"IdDb"),
	file:make_dir(ProcessRootDir),	
	{ok,Pid} = eleveldb_worker_sup:start_worker(UrlDbName,IdDbName),
	{{down_bound,DownBound},{pid,Pid}}.
% 1339670326906632 - magic Value 
    
get_env_property(PropName) ->
	case application:get_env(cache,eleveldb_env) of
		{ok,Prop} ->
					{_,Opts} = lists:keyfind(PropName,1,Prop),
					Opts;
		undefined -> undefined
	end.
			
			
