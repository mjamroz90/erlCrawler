%% @doc Serwer, ktory umozliwia wyszukiwanie w bazie domen.
%% @end

-module(domain_cache_server).
-export([start/0,lookup/1,stop/0]).
-record(state,{}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
    
-define(TABLE,domains).
  
    
%======================================API==================================
%% @type key() = string() | term()
  
%% @spec start() -> ok | {error , term()}  
%% @doc Uruchamia serwer na lokalnej maszynie.   
start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).
	
%% @spec lookup(Url :: key()) -> not_found | node()
%% @doc Wyszukuje wezel na ktorym Url powinien byc przetworzony. Jest on wyznaczany na podstawie domeny, ktora jest obliczana
%% dla tego klucza. Funkcja najpierw szuka wartosci w pamieci, jezeli tam jej nie znajdzie, siega do dysku.
%% @end.
lookup(Url) ->
	gen_server:call(?MODULE,{lookup,Url}).

%% @spec stop() -> ok
%% @doc Zatrzymuje serwer
stop() ->
	gen_server:cast(?MODULE,stop).
	
%===================================Callbacks==============================

%% @private
init([]) ->		
	{ok,#state{}}.
	
%% @private	
handle_cast(stop,State) ->
	{stop,"Made to stop",State}.

%% @private	
%% Patrzymy do Cache, pozniej do mnesii - uzywana jest operacja dirty_read, bez uzycia transkacji.
handle_call({lookup,Url},_From,State) ->
	Result = case domain_ram_cache_server:lookup(Url) of 
		not_found ->			
			case mnesia:dirty_read(domain_dispatch_server:get_domain_table_name(),Url)of
				[{domain_to_node,Url,Node}] -> Node;
				[] -> not_found
			end;	
		Node -> Node
	end,	
	{reply,Result,State}.

%% @private
handle_info(_Msg,State) ->
	{noreply,State}.

%% @private	
terminate(_Reason,_State) ->
	ok.

%% @private
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
	
