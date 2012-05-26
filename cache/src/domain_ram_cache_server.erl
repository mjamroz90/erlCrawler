-module(domain_ram_cache_server).	 
-export([start/1,insert/2,lookup/1,delete/1,stop/0,get_cache_size/0,delete_all/0]).
-define(SERVER,?MODULE).

%================================API===========================================
start(Max_Item_Number) ->
	gen_ram_cache_server:start(Max_Item_Number,?SERVER).

insert(Url,Params) ->
	gen_ram_cache_server:insert(?SERVER,Url,Params).

lookup(Url) ->
	gen_ram_cache_server:lookup(?SERVER,Url).

delete(Url) ->
	gen_ram_cache_server:delete(?SERVER,Url).	
	
delete_all() ->
	gen_ram_cache_server:delete_all(?SERVER).
		
get_cache_size() ->
	gen_ram_cache_server:get_cache_size(?SERVER).
	
stop() ->
	gen_ram_cache_server:stop(?SERVER).
