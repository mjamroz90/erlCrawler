%% @doc Generyczny serwer przechowujacy obiekty typu klucz-wartosc w pamieci RAM.
%% Mozna utworzyc ich wiele, ale musza sie roznic miedzy soba nazwa.
%% @end

-module(gen_ram_cache_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
		 
-export([start/2,insert/3,lookup/2,delete/2,stop/1,get_cache_size/1,delete_all/1]).
-record(state,{max_item_number,ram_table}).

%% @type key() = term()
%% @type value() = term()
%% @type servername() = string() | atom()

%================================API===========================================
%% @spec start(Max_Item_Number :: integer(), ServerName :: servername() ) -> ok | {error,term()}
%% @doc Uruchamia serwer o podanej nazwie, oraz maksymalna liczba kluczy przechowywanych w pamieci.
start(Max_Item_Number,ServerName) ->
	gen_server:start_link({local,ServerName},?MODULE,[Max_Item_Number,ServerName],[]).

%% @spec insert(ServerName :: servername(), Url :: key(), Params :: value()) -> ok
%% @doc Deleguje wstawienie obiektu klucz-wartosc do serwera o podanej nazwie.  
insert(ServerName,Url,Params) ->
	gen_server:cast(ServerName,{insert,{Url,Params}}).

%% @spec lookup(ServerName :: servername(), Url :: key()) -> value() | not_found
%% @doc Deleguje wyszukanie klucza do serwera o podanej nazwie.
lookup(ServerName,Url) ->
	gen_server:call(ServerName,{lookup,Url}).

%% @spec delete(ServerName :: servername(), Url :: key()) -> ok
%% @doc Deleguje usuniecie obiektu o danym kluczu do serwera o podanej nazwie.
delete(ServerName,Url) ->
	gen_server:cast(ServerName,{delete,Url}).	
	
%% @spec delete_all(ServerName :: servername()) -> ok
%% @doc Deleguje operacje usuniecia wszystkich obiektow do serwera o podanej nazwie.	
delete_all(ServerName) ->
	gen_server:cast(ServerName,delete_all).
	
%% @spec get_cache_size(ServerName :: servername()) -> integer()
%% @doc Zwraca liczbe wstawionych kluczy dla serwera o danej nazwie.		
get_cache_size(ServerName) ->
	gen_server:call(ServerName,get_cache_size).
	
	
%% @spec stop(ServerName :: servername()) -> ok
%% @doc Zatrzymuje serwer o podanej nazwie, usuwajac z niego wszystkie klucze.	
stop(ServerName) ->
	gen_server:cast(ServerName,stop).
	
%===============================Callbacks======================================

%% @private
init([Max_Item_Number,ServerName]) ->
	State = #state{max_item_number=Max_Item_Number,ram_table=ram_cache:new(ServerName)},
	{ok,State}.

%% @private
handle_cast({insert,{Url,Params}},State = #state{max_item_number=Max,ram_table=Ram_Table}) ->
	case ram_cache:tab_size(Ram_Table) == Max of
		true -> ram_cache:remove_random(Ram_Table),
				ram_cache:insert(Ram_Table,Url,Params);				
		false -> ram_cache:insert(Ram_Table,Url,Params)
	end,
	{noreply,State};

handle_cast(delete_all, State = #state{ram_table=Ram_Table}) ->
	ram_cache:delete_all(Ram_Table),
	{noreply,State};
	
handle_cast({delete,Url},State = #state{ram_table=Ram_Table}) ->	
	case ram_cache:exists(Ram_Table,Url) of 
		true -> ram_cache:delete(Ram_Table,Url);				
		false -> ok
	end,
	{noreply,State};

handle_cast(stop,State) ->
	{stop,"Made to stop",State}.

%% @private
handle_call({lookup,Url},_From,State=#state{ram_table=Ram_Table}) ->
	Result = case ram_cache:lookup(Ram_Table,Url) of
		[] -> not_found;
		Value -> Value
	end,
	{reply,Result,State};
	
handle_call(get_cache_size,_From,State=#state{ram_table=Ram_Table}) ->	
	{reply,ram_cache:tab_size(Ram_Table),State}.
	
%% @private	
handle_info(_Msg,State) ->
	{noreply,State}.

%% @private
terminate(_Reason,_State = #state{ram_table=Ram_Table}) ->
	ram_cache:remove(Ram_Table),
	ok.
	
%% @private	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
