%% @doc Serwer przechowujacy klucze, ktore nie zostaly przetworzone w biezacej sesji Crawlu. Sluzy on do wyciagania z bazy adresow, ktore powinny isc do przetworzenia.
%% @end

-module(visited_urls_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start/0, insert/1, pull_urls/2, stop/0, delete/1]).
-define(ROOT_DIR,get_env_property(root_dir)).
-define(DB_NAME,"visited").
-record(state,{visitedurl_db_ref}).
-define(OPEN_OPTS,get_env_property(open_opts)).
-define (WRITE_OTPS, [{sync, false}]).
 
%% @type key() = string() | term()
 
%% @spec start() -> {ok,Pid :: pid()} | {error,term()}
%% @doc Uruchamia proces serwera. Jezeli baza nie zostala jeszcze stworzona, to dzieje sie to teraz.  
start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[filename:join(?ROOT_DIR,?DB_NAME)],[]).
	
%% @spec insert(Id :: integer()) -> ok | {error,term()}
%% @doc Wstawia do bazy adres o danym identyfikatorze. Identyfikator pochodzi z glownej bazy url-i.
insert(Id) ->
	gen_server:call(?MODULE,{insert,Id}).

%% @spec delete(Id :: integer()) -> ok | {error,term()}
%% @doc Usuwa z bazy adres o danym identyfikatorze.
delete(Id) ->
	gen_server:call(?MODULE,{delete,Id}).

%% @spec pull_urls(Count :: integer(), From :: pid()) -> [key()]
%% @doc Wydobywa z bazy nieprzetworzone adresy w ilosc Count, lub mniejszej, jezeli tylu ich tam nie ma i zwraca odpowiedz do procesu o pid-ie From.
pull_urls(Count,From) ->
	gen_server:cast(?MODULE,{pull_urls,{Count,From}}).

%% @doc Zatrzymuje proces serwera.
stop() ->
	gen_server:cast(?MODULE,stop).

%===================================Callbacks=============================

%% @private
init([VisitedUrlDbName]) ->	
	case eleveldb:open(VisitedUrlDbName,?OPEN_OPTS) of
		{ok,VisitedUrlDb_Ref} -> 
						{ok,#state{visitedurl_db_ref = VisitedUrlDb_Ref}};
		{error,Reason} -> {stop,Reason}
	end.

%% @private
handle_cast({pull_urls,{Count,From}},State = #state{visitedurl_db_ref = VisitedUrlDb_Ref}) ->
    Result = case eleveldb:iterator(VisitedUrlDb_Ref,[]) of
        {ok,ItrRef} ->
            case eleveldb:iterator_move(ItrRef,first) of
                {ok,BinKey,_Value} ->
                    List = collect_keys(ItrRef,1,Count,[binary_to_term(BinKey)]),
                    remove_keys(List,VisitedUrlDb_Ref),
                    %lists:map(fun(Id) -> eleveldb_disk_cache_server:get_url_by_id(Id) end,List);
                    lists:map(fun(Id) -> disk_cache_server:get_url_by_id(Id) end,List);
                {error,_ } ->	[]
            end;
        {error,_} -> []
    end,
    gen_server:reply(From,Result),
    {noreply,State};

handle_cast(stop,State) ->
	{stop, "Made to stop", State}.

%% @private	
handle_call({insert,Id},_From,State = #state{visitedurl_db_ref = VisitedUrlDb_Ref}) ->	
	Result = case eleveldb:put(VisitedUrlDb_Ref,term_to_binary(Id),term_to_binary([]), ?WRITE_OTPS) of
		ok -> ok;		 				
		{error,Reason} -> {error,Reason}
	end,	
	{reply,Result,State};
	
handle_call({delete,Id},_From,State = #state{visitedurl_db_ref = VisitedUrlDb_Ref}) ->	
	Result = eleveldb:delete(VisitedUrlDb_Ref,term_to_binary(Id),[{sync,false}]),
	{reply,Result,State};


handle_call(_Msg,_From,State) ->
	{reply,ok,State}.

%% @private	
handle_info(_Msg,State) ->
	{noreply,State}.

%% @private
terminate(_Reason, _State) ->	
	ok.

%% @private
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

 
%=====================================Internal=================================

remove_keys(List,VisitedUrlDb_Ref) ->
	lists:foreach(fun(Key) -> eleveldb:delete(VisitedUrlDb_Ref,term_to_binary(Key),[{sync,false}]) end,List).
	
collect_keys(It,Max,Max,List) ->
	eleveldb:iterator_close(It),
	List;

collect_keys(It,Count,Max,List) ->
	case eleveldb:iterator_move(It,next) of
		{ok,Key,_Value} -> collect_keys(It,Count+1,Max,[binary_to_term(Key) | List]);
		{error,_} -> List
	end.	
	
get_env_property(PropName) ->
	case application:get_env(cache,eleveldb_env) of
		{ok,Prop} ->
					{_,Opts} = lists:keyfind(PropName,1,Prop),
					Opts;
		undefined -> undefined
	end.	
 
