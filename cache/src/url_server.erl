%% @doc Modul Stanowiacy interfejs dostepu do bazy Url-i. Zewnetrzne podystsemy powinny z niego korzystac.
%% @end

-module(url_server).
-export([insert/2,lookup/1, update/2]).

%================================API===========================================

%% @type url() = string()
%% @type proplist() = [{Key::term(), Value::term()}]

%% @spec insert(Url :: url(), Params :: proplist()) -> ok
%% @doc Wstawia pare klucz-wartosc do bazy.
insert(Url,Params) ->
	insert1(Url,Params).

%% @spec update(Url :: url(), Params :: proplist()) -> ok
%% @doc Aktualizuje liste parametrow dla podanego adresu.	
update(Url, Params) ->
	update1(Url,Params).
	
%% @spec lookup(Url :: url()) -> proplist()
%% @doc Zwraca liste parametrow dla podanego adresu.
lookup(Url) ->
	lookup1(Url).

%===============================Callbacks======================================


insert1(Url,Params) ->
	case disk_cache_server:insert(Url, Params) of
		{ok,NewParams} -> ram_cache_server:insert(Url, NewParams);
		{error,_ } -> void
	end,	
	ok.
	
update1(Url, Params) ->
	case disk_cache_server:update(Url, Params) of
		{ok,NewParams} -> 
			ram_cache_server:delete(Url),
			ram_cache_server:insert(Url, NewParams);
		_ -> void
	end,		
	ok.

lookup1(Url) ->
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
	Result.

	
