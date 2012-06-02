-module(domain_manager_app).
-behaviour(application).

-export([start/2,stop/1]).
-export([do_once/0]).
-record(domain_to_node,{domain,node}).

-define(CONTACT_NODES,['michal@192.168.1.105','michal@192.168.1.103']).

start(_StartType,_StartArgs) ->	
	Nodes = case application:get_env(erlCrawler,contact_nodes) of
			{ok,Nodes1} -> Nodes1;
			undefined -> ?CONTACT_NODES
		end,	
	mnesia:start(),	
	%=trzeba dorobic mnesia:wait_for_Table wtedy kiedy bedzie stworzony schemat bazy i tabela
	mnesia:wait_for_tables([domain_dispatch_server:get_domain_table_name()],2000),
	domain_manager_sup:start(Nodes).

stop(_State) ->
	domain_dispatch_server:stop(),
	mnesia:stop(),
	ok.

do_once() ->
	mnesia:create_schema(?CONTACT_NODES),
	mnesia:start(),
	mnesia:create_table(domain_dispatch_server:get_domain_table_name(),
		[{attributes,record_info(fields,domain_to_node)},{disc_only_copies,?CONTACT_NODES}]),
	mnesia:stop().