%% @doc Modul uruchamiajacy cala aplikacje oferujaca funkcjonalnosc dostepu do danych.
%% @end

-module(cache_app).
-behaviour(application).

-export([start/2,stop/1]).

-define(CACHE_MAX_ITEM_NUM,1000000).
-define(DOMAIN_MAX_ITEM_NUM,10000).
-define(NODE_NAME,"127.0.0.1").
-define(PORT,8087).

%% @private
start(_StartType,_StartArgs) ->	
	MaxItemNumCache = case get_env_property(max_item_num_cache) of
			undefined -> ?CACHE_MAX_ITEM_NUM;
			M -> M			
		end,
	NodeName = case get_env_property(node_name) of
			undefined -> ?NODE_NAME;			N -> N
			
		end,
	Port = case get_env_property(port) of
			undefined -> ?PORT;
			P -> P			
		end,			
	MaxItemNumDomain = case get_env_property(max_item_num_domain) of
			undefined -> ?DOMAIN_MAX_ITEM_NUM;
			M1 -> M1			
		end,	
	mnesia:start(),	
	%trzeba dorobic mnesia:wait_for_Table wtedy kiedy bedzie stworzony schemat bazy i tabela 	
	main_sup:start(MaxItemNumCache,MaxItemNumDomain,NodeName,Port).

%% @private
stop(_State) ->
	ram_cache_server:stop(),
	domain_ram_cache_server:stop(),
	domain_cache_server:stop(),
	disk_cache_server:stop(),
	spawn(fun() -> mnesia:stop() end),
	ok.
	
get_env_property(PropName) ->	
	case application:get_env(cache,riak_env) of
		{ok,Prop} ->					
					{_,Opts} = lists:keyfind(PropName,1,Prop),
					Opts;
		undefined -> undefined
	end.				
