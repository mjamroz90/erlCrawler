-module(domain_manager_app).
-behaviour(application).

-export([start/2,stop/1]).
-export([do_once/0,do_once1/0]).
-record(domain_to_node,{domain,node}).

-define(CONTACT_NODES,['michal@192.168.1.103','michal@192.168.1.105']).

start(_StartType,_StartArgs) ->	
	Nodes = case application:get_env(erlCrawler,contact_nodes) of
			{ok,Nodes1} -> Nodes1;
			undefined -> ?CONTACT_NODES
		end,	
	mnesia:start(),	
	mnesia:wait_for_tables([domain_dispatch_server:get_domain_table_name()],2000),
	TargetFun = fun(Load) -> Load end,
	domain_manager_sup:start(TargetFun,Nodes).

stop(_State) ->
	domain_dispatch_server:stop(),
	mnesia:stop(),
	ok.

do_once() ->
	mnesia:create_schema(?CONTACT_NODES).
	%mnesia:start(),
	%mnesia:create_table(domain_dispatch_server:get_domain_table_name(),
		%[{attributes,record_info(fields,domain_to_node)},{disc_only_copies,?CONTACT_NODES}]).
do_once1() ->
	mnesia:create_table(domain_dispatch_server:get_domain_table_name(),
		[{attributes,record_info(fields,domain_to_node)},{disc_only_copies,?CONTACT_NODES}]).
