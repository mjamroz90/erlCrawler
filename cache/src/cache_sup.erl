%% @doc Supervisor nadzorujacy procesy zwiazane z bazu Url-i.
%% Uruchamia on 2 procesy:
%% <ul>
%% <li>  Serwer trzymajacy adresy w pamieci RAM. </li>
%% <li>  Supervisor nadzorujacy procesy trzymajace adresy na dysku. </li>
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
	%DiskCacheServer = {disk_cache_server,{disk_cache_server,start,[NodeName,Port]},
				%permanent,brutal_kill,worker,[disk_cache_server]},				
	
	EleveldbMainSup = {eleveldb_main_sup,{eleveldb_main_sup,start,[]},
					permanent,brutal_kill,supervisor,[eleveldb_main_sup]},
	
	RestartStrategy = {one_for_all,100,1},
	{ok,{RestartStrategy,[RamCacheServer,EleveldbMainSup]}}.

