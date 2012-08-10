%% @doc Glowny Supervisor nadzorujacy dwa pomniejsze :
%% <ul>
%% <li> Nadzorujacy procesy zwiazane z baza Url-i </li>
%% <li> Nadzorujacy procesy zwiazane z baza domen </li>
%% </ul>

-module(main_sup).
-behaviour(supervisor).
-export([start/4]).
-export([init/1]).

%% @type address() = atom() | string() | inet:ip_address()

%% @spec start(MaxItemNumCache :: integer(), MaxItemNumDomain :: integer(), NodeName :: address(), Port :: integer()) -> ok | {error,term()}
%% @doc <ul>
%% <li> MaxItemNumCache - maksymalna liczba kluczy w pamieci RAM dla serwera bazy Url-i. </li>
%% <li> MaxItemNumDomain - maksymalna liczba kluczy w pamieci RAM dla serwera bazy domen. </li>
%% <li> NodeName - adres, do ktorego laczy sie disk_cache_server </li>
%% <li> Port - port, do ktorego laczy sie disk_cache_server </li>
%% </ul>
%% @end
start(MaxItemNumCache,MaxItemNumDomain,NodeName,Port) ->
	supervisor:start_link({local,?MODULE},?MODULE,[MaxItemNumCache,MaxItemNumDomain,NodeName,Port]).
	
%% @private	
init([MaxItemNumCache,MaxItemNumDomain,NodeName,Port]) ->
	CacheSup = {cache_sup,{cache_sup,start,[MaxItemNumCache,NodeName,Port]},
				permanent,brutal_kill,supervisor,[cache_sup]},	
	DomainCacheSup = {domain_cache_sup,{domain_cache_sup,start,[MaxItemNumDomain]},
				permanent,brutal_kill,supervisor,[domain_cache_sup]},
					
	RestartStrategy = {one_for_one,100,1},
	{ok,{RestartStrategy,[CacheSup,DomainCacheSup]}}.


