%% @doc Serwer, ktory stanowi interfejs dostepu do bazy domen. Przydziela on zadanie przetworzenia adresu do odpowiedniego wezla.
%% @end 

-module(domain_dispatch_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
		 
-export([start/1,insert/1,update/2,stop/0,get_domain_table_name/0]).
-define(INFO_TAB,manager_info).
-define(DOMAIN_TO_NODES,domain_to_node).

-record(manager_info,{current_dispatcher,nodes_num}).
-record(state,{contact_nodes}).
-record(domain_to_node,{domain,node}).

%% @type domain() = string() | atom()

%% @spec get_domain_table_name() -> atom()
%% @doc Zwraca nazwe tabeli mnesii, w ktorej przechowywane jest odwzorowanie domena - wezel.
get_domain_table_name() ->
	?DOMAIN_TO_NODES.

%% @spec start(Nodes :: [node()]) -> ok | {error,term()}
%% @doc Uruchamia serwer, argument Nodes to lista wezlow, na ktorych dziala crawler.
start(Nodes) ->	
	gen_server:start_link({local,?MODULE},?MODULE,[Nodes],[]).

%% @spec insert(Url :: domain()) -> node()
%% @doc Wstawia adres domenowy do bazy, zarowno na dysk jak i do podrecznych cache'ow.
insert(Url) ->
	gen_server:call(?MODULE,{insert,Url}).

%% @spec update(Url :: domain(), NewNode :: node()) -> ok
%% @doc Uaktualnia wezel na ktorym maja isc adresy z tej domeny.
update(Url,NewNode) ->
	gen_server:call(?MODULE,{update,{Url,NewNode}}).
	
%% @spec stop() -> ok
%% @doc Zatrzymuje serwer	
stop() ->
	gen_server:cast(?MODULE,stop).
	
%================================CallBacks===============================

%% @private
init([Nodes]) ->
	ContactNodes = ensure_contact(Nodes),	
	mnesia:create_table(?INFO_TAB,[{ram_copies,ContactNodes},{attributes,record_info(fields,manager_info)}]),
	mnesia:dirty_write(?INFO_TAB,#manager_info{current_dispatcher=node(),nodes_num=length(ContactNodes)}),
	{ok,#state{contact_nodes = ContactNodes}}.

%% @private	
handle_call({insert,Url},_From,State = #state{contact_nodes = Nodes}) ->
	NodeName = get_node(Nodes),
	write_to_caches(Url,NodeName,Nodes),
	{reply,NodeName,State};

handle_call({update,{Url,NewNode}},_From,State = #state{contact_nodes = Nodes}) ->	
	write_to_caches(Url,NewNode,Nodes),
	{reply,ok,State}.

%% @private	
handle_cast(stop,State) ->
	{stop,"Made to Stop",State}.

%% @private
handle_info(_Msg,State) ->
	{noreply,State}.
	
%% jezeli dispatcher padnie, trzeba by rozpoczac procedure wyboru nowego wezla glownego
%% @private  	
terminate(_Reason,_State) ->	
	mnesia:dirty_write(?INFO_TAB,#manager_info{current_dispatcher=unknown}),
	ok.
	
%% @private	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
write_to_caches(Url,NodeName,Nodes) ->
	F = fun() -> mnesia:write(?DOMAIN_TO_NODES,#domain_to_node{domain=Url,node=NodeName}) end,
	mnesia:transaction(F),
	spawn (fun() -> rpc:multicall(Nodes,domain_ram_cache_server,insert,[Url,NodeName]) end).
		
%=====================Internal========================================
get_node(ContactNodes) ->
	%lists:nth(1,ContactNodes).
	Index = random:uniform(length(ContactNodes)), 
	lists:nth(Index, ContactNodes).
	
ensure_contact(Nodes) ->
	[ContactNode || ContactNode <- Nodes, net_adm:ping(ContactNode) =:= pong].
