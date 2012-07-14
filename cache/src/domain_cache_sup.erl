%% @doc Supervisor nadzorujacy procesy zwiazane z baza domen.
%% Uruchamia on 2 serwery:
%% <ul>
%% <li>  Serwer trzymajacy odwzorowanie domena-wezel w pamieci RAM. </li>
%% <li>  Serwer stanowiacy interfejs dostepu do bazy domen . </li>
%% </ul>
%% @end

-module(domain_cache_sup).
-behaviour(supervisor).
-export([start/1]).
-export([init/1]).

%% @spec start(MaxItemNum :: integer()) -> ok | {error,term()}
%% @doc Uruchamia Supervisor z parametrem:
%% <ul>
%% <li> MaxItemNum -  maksymalna ilosc przechowywanych kluczy w RAM, parametr dla ram_cache_domain_server. </li>
%% </ul> 
start(MaxItemNum) ->
	supervisor:start_link({local,?MODULE},?MODULE,[MaxItemNum]).
	
%% @private	
init([MaxItemNum]) ->
	RamCacheDomainServer = {domain_ram_cache_server,{domain_ram_cache_server,start,[MaxItemNum]},
				permanent,brutal_kill,worker,[domain_ram_cache_server]},	
	DomainCacheServer = {domain_cache_server,{domain_cache_server,start,[]},
				permanent,brutal_kill,worker,[domain_cache_server]},
	
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[RamCacheDomainServer,DomainCacheServer]}}.
