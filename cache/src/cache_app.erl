-module(cache_app).
-behaviour(application).

-export([start/2,stop/1]).

-define(MAX_ITEM_NUM,10000000).
-define(NODE_NAME,"127.0.0.1").
-define(PORT,8087).

start(_StartType,_StartArgs) ->	
	MaxItemNum = case application:get_env(erlCrawler,max_item_num) of
			{ok,M} -> M;
			undefined -> ?NODE_NAME
		end,
	NodeName = case application:get_env(erlCrawler,node_name) of
			{ok,N} -> N;
			undefined -> ?NODE_NAME
		end,
	Port = case application:get_env(erlCrawler,riak_port) of
			{ok,P} -> P;
			undefined -> ?PORT
		end,	
	case cache_sup:start(MaxItemNum,NodeName,Port) of
		{ok,Pid1} ->
			{ok,Pid1};
		Other1 ->
			{error,Other1}
	end.

stop(_State) ->
	ram_cache_server:stop(),
	disk_cache_server:stop(),
	ok.
