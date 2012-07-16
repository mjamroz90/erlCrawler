%% @doc Serwer zbierajacy informacje o obciazeniu poszczegolnych wezlow. Wykorzystuje je do rownowazenia obciazenia w przydzielaniu
%% adresow do przetwarzania na poszczegolne wezly.
%% @end

-module(load_collector_server).
-behaviour(gen_server).
-export([init/1,handle_cast/2,code_change/3,terminate/2,handle_info/2,handle_call/3]).
-export([start/2,stop/0,report_load/2,replace_target_fun/1,get_least_charged_node/0]).
-record(state,{target_fun,node_load}).
-define(HANDLER,load_collector_handler).
-define(CRAWL_EVENT,crawl_event).

%% @type proplist() = [{Key :: term(), Value :: term()}]

%% @spec start(TargetFun :: function(), Nodes :: [node()]) -> ok | {error,term()}
%% @doc Uruchamia serwer zbierajacy obciazenie. Argumenty to:
%% <ul>
%% <li> TargetFun - funkcja wyliczajaca wartosc liczbowa z parametrow okreslajacych obciazenie. </li>
%% <li> Nodes - wezly, z ktorych zbierane jest obciazenie. </li>
%% </ul>
start(TargetFun,Nodes) ->
	gen_server:start_link({local,?MODULE},?MODULE,[TargetFun,Nodes],[]).

%% @spec stop() -> ok
%% @doc Zatrzymuje serwer
stop() ->
	gen_server:cast(?MODULE,stop).
	
%% @spec report_load(Node :: node(), Load :: proplist()) -> ok
%% @doc Podaje serwerowi liste parametrow okreslajacych obciazenie wezla.
report_load(Node,Load) ->
	gen_server:cast(?MODULE,{report_load,{Node,Load}}).
	
%% @spec replace_target_fun(NewFun :: function()) -> OldTargetFun :: function()
%% @doc Podmienia funkcje celu.	
replace_target_fun(NewFun) ->
	gen_server:call(?MODULE,{replace_target,NewFun}).
	
%% @spec get_least_charged_node() -> node()
%% @doc Zwraca nazwe najmniej obciazonego wezla.	
get_least_charged_node() ->
	gen_server:call(?MODULE,get_least_charged_node).
	
%==================================Callbacks=====================================
	
%% @private	
init([TargetFun,Nodes]) ->	
	ContactNodes = ensure_contact(Nodes),
	lists:map(fun(Node) -> gen_event:add_handler({?CRAWL_EVENT,Node},?HANDLER,[node()]) end,ContactNodes),
	{ok,#state{target_fun = TargetFun,node_load = []}}.	

%% @private
handle_call(get_least_charged_node,_From,State = #state{node_load = NodeLoad}) ->
	Node = case NodeLoad of
		[] -> no_nodes;
		_ -> [{Node1,_} | _ ] = lists:keysort(2,NodeLoad),Node1
	end,	
	{reply,Node,State};

handle_call({replace_target_fun,NewTargetFun},_From,State = #state{target_fun = OldTargetFun}) ->
	{reply,OldTargetFun,State#state{target_fun = NewTargetFun}}.

%% @private			
handle_cast({report_load,{Node,Load}},State = #state{target_fun = TargetFun,node_load = NodeLoad}) ->
	NewList = case lists:keyfind(Node,1,NodeLoad) of
		false -> [{Node,TargetFun(Load)} | NodeLoad];
		_ -> lists:keyreplace(Node,1,NodeLoad,{Node,TargetFun(Load)})		
	end,
	{noreply,State#state{node_load = NewList}};

handle_cast(stop,State) ->
	{stop,"Made to stop",State}.

%% @private
handle_info(_Info,State) ->
	{ok,State}.

%% @private	
terminate(_Arg,#state{node_load = NodeLoad}) ->
	lists:foreach(fun({Node,_}) -> gen_event:delete_handler({?CRAWL_EVENT,Node},?HANDLER,[]) end,NodeLoad),
	ok.

%% @private	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

%================================Internal========================================
	
ensure_contact(Nodes) ->
	[ContactNode || ContactNode <- Nodes, net_adm:ping(ContactNode) =:= pong].
