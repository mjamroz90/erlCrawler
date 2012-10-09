%% @doc Interfejs dostepu do bazy adresow na dysku.
%% @end

-module(disk_cache_server).
-define(CURRENT_MOD,eleveldb_disk_cache_server).
-export([insert/2,lookup/1,update/2,delete/1,pull_urls/1,get_param/2, get_url_by_id/1, get_session_id/0
		]).
%% @type key() = string() | term()
%% @type proplist() = [{Key::term(), Value::term()}]   
%% @type address() = atom() | string() | inet:ip_address()

%% @spec insert(Url :: key(), Params :: proplist()) -> {ok,NewParams :: proplist()} | {error,term()}
%% @doc Wstawia do bazy na dysku pare klucz wartosc.		
insert(Url,Params) ->
	?CURRENT_MOD:insert({get_session_id(), Url},Params).
	
%% @spec lookup(Url :: key()) -> proplist() | not_found
%% @doc Szuka wartosci dla podanego klucza.	
lookup(Url) ->
	?CURRENT_MOD:lookup({get_session_id(), Url}).
	
%% @spec update(Url :: key(), NewParams :: proplist()) -> {ok,NewParams :: proplist()} | not_found | {error,term()}	
%% @doc Aktualizuje obiekt klucz-wartosc.
update(Url,NewParams) ->
	?CURRENT_MOD:update({get_session_id(), Url},NewParams).
	
%% @spec delete(Url :: key() ) -> 	ok | {error,term()}
%% @doc Usuwa obiekt o podanym kluczu.
delete(Url) ->
	?CURRENT_MOD:delete({get_session_id(), Url}).
	
%% @spec pull_urls(Count :: integer()) -> [key()]
%% @doc Zwraca klucze o dlugosci Count, ktore sa nieprzetworzone. Jezeli w bazie nie ma tylu nieprzetworzonych kluczy, to zostanie zwroconych tyle ile jest.
pull_urls(Count) ->
	?CURRENT_MOD:pull_urls(Count).

%% @spec get_param(ParamName :: atom(),ParamList :: proplist()) -> term() | not_found
%% @doc Zwraca wartosc danego property  na liscie properties.
get_param(ParamName,ParamList) ->
	riak_disk_cache_server:get_param(ParamName,ParamList).

%% @spec get_url_by_id(Id :: integer()) -> key()
%% @doc Zwraca adres dla podanego identyfikatora.	
get_url_by_id(Id) ->
	case ?CURRENT_MOD:get_url_by_id(Id) of
		not_found -> not_found;
		{_SessionId, Url} -> Url
	end.


%% @spec get_session_id() -> integer()
%% @doc Zwraca aktualny identyfikator sesji	
get_session_id() ->
	SessionId = case application:get_env(cache, session_id) of
		{ok, Val} -> Val;
		undefined -> 0
	end,
	SessionId.
	
