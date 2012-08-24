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

    MsgContent = io_lib:format("Aplikacja cache wystartowala z nastepujacymi parametrami dotyczacymi pamieci RAM :\n
    - Cache dla bazy Url-i : ~p elementow \n
    - Cache dla bazy domen : ~p elementow",[MaxItemNumCache,MaxItemNumDomain]),

    crawl_event:log_message({info,node(),cache,start,lists:flatten(MsgContent)}),
	main_sup:start(MaxItemNumCache,MaxItemNumDomain,NodeName,Port).

%% @private
stop(_State) ->
    MsgContent = "Aplikacja cache zostala zatrzymana\n",
    crawl_event:log_message({info,node(),cache,stop,MsgContent}),
	ok.
	
get_env_property(PropName) ->	
	case application:get_env(cache,riak_env) of
		{ok,Prop} ->					
					{_,Opts} = lists:keyfind(PropName,1,Prop),
					Opts;
		undefined -> undefined
	end.				
