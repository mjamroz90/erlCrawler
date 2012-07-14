%% @doc Serwer przechowujacy odwzorowanie domena-wezel w pamieci RAM.
%% @end

-module(domain_ram_cache_server).	 
-export([start/1,insert/2,lookup/1,delete/1,stop/0,get_cache_size/0,delete_all/0]).
-define(SERVER,?MODULE).

%================================API===========================================
%% @type domain() = string()

%% @spec start(Max_Item_Number :: integer()) -> ok | {error,term()}
%% @doc Uruchamia serwer z parametrem okreslajacym maksymalna ilosc przechowywanych kluczy
start(Max_Item_Number) ->
	gen_ram_cache_server:start(Max_Item_Number,?SERVER).

%% @spec insert(Url :: domain(), Params :: node()) -> ok
%% @doc Wstawia obiekt klucz-wartosc.
insert(Url,Params) ->
	gen_ram_cache_server:insert(?SERVER,Url,Params).

%% @spec lookup(Url :: domain()) -> node()
%% @doc Wyszukuje wezel dla danego klucza.
lookup(Url) ->
	gen_ram_cache_server:lookup(?SERVER,Url).

%% @spec delete(Url :: domain()) -> ok
%% @doc Usuwa pare klucz-wartosc o podanym kluczu.
delete(Url) ->
	gen_ram_cache_server:delete(?SERVER,Url).	

%% @spec delete_all() -> ok	
%% @doc Usuwa wszystkie klucze.
delete_all() ->
	gen_ram_cache_server:delete_all(?SERVER).
	
%% @spec get_cache_size() -> integer()
%% @doc Zwraca liczbe aktualnie przechowywanych kluczy.		
get_cache_size() ->
	gen_ram_cache_server:get_cache_size(?SERVER).
	
%% @spec stop() -> ok
%% @doc Zatrzymuje serwer, usuwajac tym samym wszystkie klucze.
stop() ->
	gen_ram_cache_server:stop(?SERVER).
