%% @doc Serwer wykonujacy bezposrednio operacje na bazie. Ich liczba jest okreslana konfiguracyjnie.
%% @end

-module(eleveldb_worker).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start/2, insert/4, update/4, delete/3, lookup/3, stop/1, get_url_by_id/2]).
-record(state,{url_db_ref,id_db_ref}).
-define(OPEN_OPTS,get_env_property(open_opts)).					
-define (WRITE_OTPS, [{sync, false}]).
 
%% @type key() = string() | term()
%% @type proplist() = [{Key::term(), Value::term()}]   
  
%% @spec start(UrlDbName :: string(), IdDbName :: string()) -> {ok, Pid :: pid()} | {error, term()}
%% @doc Uruchamia serwer, ktory otwiera polaczenie z baza. Jezeli bazy nie istnieja, to je tworzy.    
start(UrlDbName,IdDbName) ->
	gen_server:start_link(?MODULE,[UrlDbName,IdDbName],[]).

%% @spec insert(Pid :: pid(), From :: pid(), Url :: key(), Params :: proplist()) -> void
%% @doc Deleguje wstawienie do bazy obiektu klucz - wartosc procesowi o danym Pid-ie. From - to pid procesu, ktory zlecal wstawienie do bazy procesowi eleveldb_disk_cache_server.
%% Do niego zostanie wyslany rezulatat operacji.
insert(Pid,From,Url,Params) ->
	gen_server:cast(Pid,{insert,{From,Url,Params}}).
	
%% @spec update(Pid :: pid(), From :: pid(), Url :: key(), Params :: proplist()) -> void
%% @doc Mechanizm podobny jak w przypadku wstawiania, tylko operacja jest aktualizacja.
update(Pid,From,Url,Params) ->
	gen_server:cast(Pid,{update,{From,Url,Params}}).

%% @spec delete(Pid :: pid(), From :: pid(), Url :: key()) -> void
%% @doc Mechanizm podobny jak w przypadku wstawiania, tylko operacja jest usuwanie obiektu.
delete(Pid,From,Url) ->
	gen_server:cast(Pid,{From,delete,Url}).

%% @spec lookup(Pid :: pid(), From :: pid(), Url :: key()) -> void
%% @doc Mechanizm podobny jak w przypadku wstawiania, tylko operacja jest wyszukiwanie obiektu.	
lookup(Pid,From,Url) ->
	gen_server:cast(Pid,{From,lookup,Url}).

%% @spec get_url_by_id(Pid :: pid(), Id :: integer()) -> Url | not_found
%% @doc Szuka adresu na podanym serwerze o podanym identyfikatorze.
get_url_by_id(Pid,Id) ->
	gen_server:call(Pid,{get_url_by_id,Id}).

%% @spec stop(Pid :: pid()) -> ok
%% @doc Zatrzymuje proces o podanym pid-ie.	
stop(Pid) ->
	gen_server:cast(Pid,stop).

%===================================Callbacks=============================

%% @private
init([UrlDbName,IdDbName]) ->	
	case eleveldb:open(UrlDbName,?OPEN_OPTS) of
		{ok,UrlDb_Ref} -> 
						case eleveldb:open(IdDbName,?OPEN_OPTS) of
							{ok,IdDb_Ref} -> {ok,#state{url_db_ref = UrlDb_Ref,id_db_ref = IdDb_Ref}};
							{error, Reason1} -> {stop,Reason1}
						end;	
		{error,Reason} -> {stop,Reason}
	end.	

%% @private	
handle_cast(stop,State) ->
	{stop, "Made to stop", State};
	
handle_cast({insert,{From,Url,Params}},State = #state{url_db_ref = UrlDb_Ref, id_db_ref = IdDb_Ref}) ->
	Id = generate_id(),
	NewParams = stick_params({id,Id},Params),
	Result = case eleveldb:put(UrlDb_Ref,term_to_binary(Url),term_to_binary(NewParams), ?WRITE_OTPS) of
		ok -> case eleveldb:put(IdDb_Ref,term_to_binary(Id),term_to_binary(Url), ?WRITE_OTPS) of
				ok -> {ok, NewParams};
				{error,Reason1} -> {error,Reason1}
			  end;		 				
		{error,Reason} -> {error,Reason}
	end,
	gen_server:reply(From,Result),
	{noreply,State};
	
handle_cast({update,{From,Url,Params}},State = #state{url_db_ref = Db_Ref}) ->
	Result = case eleveldb:get(Db_Ref, term_to_binary(Url),[]) of 
		{ok, Bin_Value} -> 
						eleveldb:delete(Db_Ref,term_to_binary(Url), [{sync,false}]),
						Id = get_param(id,binary_to_term(Bin_Value)),
						NewParams = stick_params({id,Id},Params),
						case eleveldb:put(Db_Ref,term_to_binary(Url), term_to_binary(NewParams),?WRITE_OTPS) of
							ok -> {ok, NewParams};
							{error,Reason} -> {error,Reason}
						end;
												
		not_found -> not_found
	end,
	gen_server:reply(From,Result),
	{noreply,State};
	
handle_cast({From,delete,Url},State = #state{url_db_ref = UrlDb_Ref, id_db_ref = IdDb_Ref}) ->
	{ok,Bin_Value} = eleveldb:get(UrlDb_Ref, term_to_binary(Url),[]),
	Id = get_param(id,binary_to_term(Bin_Value)),
	Result = eleveldb:delete(UrlDb_Ref,term_to_binary(Url),[{sync,false}]),
	eleveldb:delete(IdDb_Ref,term_to_binary(Id),[{sync,false}]),
	gen_server:reply(From,Result),
	{noreply,State};
	
handle_cast({From,lookup,Url},State = #state{url_db_ref = Db_Ref}) ->
	Result = case eleveldb:get(Db_Ref, term_to_binary(Url), []) of
		{ok,Bin_Value} -> binary_to_term(Bin_Value);
		_ -> not_found
	end,
	gen_server:reply(From,Result),
	{noreply,State}.

%% @private
handle_call({get_url_by_id,Id},_From,State = #state{id_db_ref = IdDb_Ref}) ->
	Result = case eleveldb:get(IdDb_Ref,term_to_binary(Id),[]) of
		{ok,Bin_Value} -> binary_to_term(Bin_Value);
		_ -> not_found
	end,
	{reply,Result,State}.

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

% 1339670326906632 - magic Value 
generate_id() ->
	{Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro - 1339670326906632.
  
stick_params(NewTuple,Params) ->	
	NewParams = lists:keydelete(element(1,NewTuple),1,Params),
	[NewTuple | NewParams].
  
get_param(ParamName,ParamList) ->
	case lists:keyfind(ParamName,1,ParamList) of
		false -> not_found;
		{_,Value} -> Value
	end.     
	
get_env_property(PropName) ->
	case application:get_env(cache,eleveldb_env) of
		{ok,Prop} ->
					{_,Opts} = lists:keyfind(PropName,1,Prop),
					Opts;
		undefined -> undefined
	end.	   
