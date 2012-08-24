%% @doc Modul uruchamiajacy aplikacje bazy domen
%% @end

-module(domain_manager_app).
-behaviour(application).

-export([start/2,stop/1]).
-export([do_once/1,do_once1/1]).
-record(domain_to_node,{domain,node}).

-define(CONTACT_NODES,[node()]).

%% @doc Uruchamia aplikacje, uruchamiana jest baza mnesia, funkcja czeka na to az tabela bedzie dostepna.  
start(_StartType,_StartArgs) ->	
	Nodes = case application:get_env(session_manager,contact_nodes) of
			{ok,Nodes1} -> Nodes1;
			undefined -> ?CONTACT_NODES
		end,	
	%% Teraz mnesia uruchamiana jest w session_manager_server - mnesia:start(),
	mnesia:wait_for_tables([domain_dispatch_server:get_domain_table_name()],infinity),	
	TargetFun = fun(Load) -> Load end,
    crawl_event:log_message({info,node(),domain_manager,start,prepareMsgContent()}),
	domain_manager_sup:start(TargetFun,Nodes).

%% @doc Zatrzymuje aplikacje
stop(_State) ->
    MsgContent = "Aplikacja domain_manager zostala zatrzymana\n",
    crawl_event:log_message({info,node(),cache,stop,MsgContent}),
	ok.

%% @doc Tworzy schemat bazy na wezlach.
do_once(Nodes) ->
	mnesia:create_schema(Nodes).
	%mnesia:start(),
	%mnesia:create_table(domain_dispatch_server:get_domain_table_name(),
		%[{attributes,record_info(fields,domain_to_node)},{disc_only_copies,?CONTACT_NODES}]).

%% @doc Tworzy tabele na wezlach.		
do_once1(Nodes) ->
	mnesia:create_table(domain_dispatch_server:get_domain_table_name(),
		[{attributes,record_info(fields,domain_to_node)},{disc_only_copies,Nodes}]).

prepareMsgContent() ->
    lists:flatten(io_lib:format("Aplikacja domain_manager zostala uruchomiona na wezle ~p\n",[node()])).
