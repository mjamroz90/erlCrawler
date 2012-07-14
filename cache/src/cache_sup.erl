%% @doc Supervisor nadzorujacy procesy zwiazane z bazu Url-i.
%% Uruchamia on 3 serwery:
%% <ul>
%% <li>  Serwer trzymajacy adresy w pamieci RAM. </li>
%% <li>  Serwer trzymajacy adresy na dysku. </li>
%% <li>  Serwer, ktory stanowi glowny interfejs dostepu do bazy Url-i, korzysta z dwoch wyzej wymienionych. </li>
%% </ul>
%% @end

-module(cache_sup).
-behaviour(supervisor).
-export([start/3]).
-export([init/1]).

%% @type address() = atom() | string() | inet:ip_address()

%% @spec start(MaxItemNum :: integer(), NodeName :: address(), Port :: integer()) -> ok | {error, term()}
%% @doc Uruchamia Supervisor z parametrami:
%% <ul>
%% <li> MaxItemNum - maksymalna ilosc przechowywanych kluczy w RAM, parametr dla ram_cache_server. </li>
%% <li> NodeName,Port - parametry dla disk_cache_server. </li>
%% </ul>
%% @end
start(MaxItemNum,NodeName,Port) ->
	supervisor:start_link({local,?MODULE},?MODULE,[MaxItemNum,NodeName,Port]).

%% @private	
init([MaxItemNum,NodeName,Port]) ->
	RamCacheServer = {ram_cache_server,{ram_cache_server,start,[MaxItemNum]},
				permanent,brutal_kill,worker,[ram_cache_server]},	
	DiskCacheServer = {disk_cache_server,{disk_cache_server,start,[NodeName,Port]},
				permanent,brutal_kill,worker,[disk_cache_server]},
	UrlServer = {url_server,{url_server,start,[]},
				permanent,brutal_kill,worker,[url_server]},
	
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[RamCacheServer,DiskCacheServer,UrlServer]}}.

