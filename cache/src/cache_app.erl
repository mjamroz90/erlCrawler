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
	MaxItemNumCache = case application:get_env(erlCrawler,max_item_num_cache) of
			{ok,M} -> M;
			undefined -> ?CACHE_MAX_ITEM_NUM
		end,
	NodeName = case application:get_env(erlCrawler,node_name) of
			{ok,N} -> N;
			undefined -> ?NODE_NAME
		end,
	Port = case application:get_env(erlCrawler,riak_port) of
			{ok,P} -> P;
			undefined -> ?PORT
		end,			
	MaxItemNumDomain = case application:get_env(erlCrawler,max_item_num_domain) of
			{ok,M1} -> M1;
			undefined -> ?DOMAIN_MAX_ITEM_NUM
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
