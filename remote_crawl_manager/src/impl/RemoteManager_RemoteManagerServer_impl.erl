%% @doc Serwer implementujacy interfejs IDL. Jest to odpowiednik servant'a w implementacji CORB-y w jezykach obiektowych.

-module('RemoteManager_RemoteManagerServer_impl').
-include_lib("orber/include/corba.hrl").
-include_lib("../generated/RemoteManager.hrl").

-export([init/1, startCrawlerOnNode/3, startCrawlerOnNodes/2, startSessionOnNode/3, startSessionOnNodes/2, pingNode/2, pingApp/3,
stopSessionOnNode/2, stopSessionOnNodes/2, stopCrawlerOnNode/2,stopCrawlerOnNodes/2,terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% @type state() = tuple()
%% @type proplist() = [{Key :: string(), Value :: string()}]
%% @type nodeProperty() = {NodeName :: string(), PropList :: proplist()}

%% @private
init([]) ->
    {ok, #state{}}.

%% @spec startCrawlerOnNode(State :: state(), Node :: string(), PropertyList :: proplist()) -> proplist()
%% @doc Uruchamia cala infrastrukture crawler'a na podanym wezle, tzn. serwery baz, aplikacje przetwarzajaca zdarzenia itd.
%% Zwraca liste wynikow, ktore zostaly zwrocone przy starcie przez poszczegolne aplikacje.
startCrawlerOnNode(State,Node,PropertyList) ->
    Reply = start_crawler_on_node(Node,PropertyList),
    {reply, Reply, State}.

%% @spec startCrawlerOnNodes(State :: state(), NodesProperties :: [nodeProperty()]) -> [nodeProperty()]
%% @doc Uruchamia infrastrukture crawler'a na wezlach z podanej listy.
%% Zwraca liste wynikow, ktore zostaly zwrocone przy starcie przez poszczegolne aplikacje na tych wezlach.
startCrawlerOnNodes(State, NodesProperties) ->
    Reply = lists:map(fun(Rec = #'RemoteManager_nodeProperty'{nodeName=Node,propList=PropList}) ->
        Rec#'RemoteManager_nodeProperty'{nodeName = Node,propList = start_crawler_on_node(Node,PropList)} end,
        NodesProperties),
    %% Reply1 = lists:map(fun({Node,PropList}) -> {atom_to_list(Node),serialize_to_proplist(PropList)} end,Reply),
    {reply, Reply, State}.

%% @spec startSessionOnNode(State :: state(), Node :: string(), PropertyList :: proplist()) -> proplist()
%% @doc Uruchamia sesje crawlu z podanymi parametrami na danym wezle.
%% Zwraca liste z wynikiem zwroconym przez apliakcje scheduler'a.
startSessionOnNode(State,Node,PropertyList) ->
    Reply = start_session_on_node(Node,PropertyList),
    {reply,Reply,State}.

%% @spec startSessionOnNodes(State :: state(), NodesProperties :: [nodeProperty()]) -> [nodeProperty()]
%% @doc Uruchamia sesje crawlu z podanymi parametrami na wezlach z podanej listy.
%% Zwraca liste wynikow, ktore zostaly zwrocone przy starcie przez poszczegolne aplikacje scheduler'a na tych wezlach.
startSessionOnNodes(State, NodesProperties) ->
    Reply = lists:map(fun(Rec = #'RemoteManager_nodeProperty'{nodeName=Node,propList=PropList}) ->
        Rec#'RemoteManager_nodeProperty'{nodeName = Node,propList = start_session_on_node(Node,PropList)} end,
        NodesProperties),
    %% Reply1 = lists:map(fun({Node,PropList}) -> {atom_to_list(Node),serialize_to_proplist(PropList)} end,Reply),
    {reply, Reply, State}.

%% @spec pingNode(State :: state(), NodeName :: staring()) -> {bool(), Msg :: string()}
%% @doc Sprawdza czy podany wezel jest dostepny.
pingNode(State, NodeName) ->
    Result = case net_adm:ping(list_to_atom(NodeName)) of
        pong -> true;
        _ -> false
    end,
    {reply,{Result,""}, State}.

%% @spec pingApp(State :: state(), NodeName :: string(), AppName :: string()) -> {bool(), Msg :: string()}
%% @doc Sprawdza, czy aplikacja o podanej nazwie jest uruchomiona na podanym wezle.
pingApp(State, NodeName, AppName) ->
    Result = rpc:call(list_to_atom(NodeName),application,which_applications,[]),
    List = lists:filter(fun({App,_,_}) -> App =:= list_to_atom(AppName) end,Result),
    Reply = case length(List) > 0 of
        true -> {true,element(2,lists:nth(1,List))};
        false -> {false,""}
    end,
    {reply,Reply,State}.

%% @spec stopSessionOnNode(State :: state(), Node :: string()) -> proplist()
%% @doc Analogicznie jak funkcja startSessionOnNode/3.
%% @see startSessionOnNode/3.
stopSessionOnNode(State, Node) ->
    Reply = rpc:call(list_to_atom(Node),session_manager_server,stop_session,[]),
    {reply,serialize_to_proplist(Reply), State}.

%% @spec stopSessionOnNodes(State :: state(), NodeList :: [string()]) -> [nodeProperty()]
%% @doc Analogicznie jak funkcja startSessionOnNodes/2.
%% @see startSessionOnNodes/2.
stopSessionOnNodes(State, NodeList) ->
    {Results,_} = rpc:multicall(lists:map(fun(Node) -> list_to_atom(Node) end,NodeList),session_manager_server,stop_session,[]),
    Reply = lists:map(fun(Num) -> {lists:nth(Num,NodeList),lists:nth(Num,Results)} end,lists:seq(1,length(Results))),
    Reply1 = lists:map(fun({Node,PropList}) -> #'RemoteManager_nodeProperty'{nodeName = Node,
                            propList = serialize_to_proplist(PropList)} end,Reply),
    {reply,Reply1,State}.

%% @spec stopCrawlerOnNode(State :: state(), NodeList :: [string()]) -> proplist()
%% @doc Analogicznie jak funkcja startCrawlerOnNode/3.
%% @see startCrawlerOnNode/3.
stopCrawlerOnNode(State, NodeName) ->
    Result = rpc:call(list_to_atom(NodeName), session_manager_server,stop_crawler,[]),
    {reply,serialize_to_proplist(Result), State}.

%% @spec stopCrawlerOnNodes(State :: state(), NodeList :: [string()]) -> [nodeProperty()]
%% @doc Analogicznie jak funkcja startCrawlerOnNodes/2.
%% @see startCrawlerOnNodes/2.
stopCrawlerOnNodes(State, NodeList) ->
    {Results,_} = rpc:multicall(lists:map(fun(Node) -> list_to_atom(Node) end,NodeList),session_manager_server,stop_crawler,[]),
    Reply = lists:map(fun(Num) -> {lists:nth(Num,NodeList),lists:nth(Num,Results)} end,lists:seq(1,length(Results))),
    Reply1 = lists:map(fun({Node,PropList}) -> #'RemoteManager_nodeProperty'{nodeName = Node,
                            propList = serialize_to_proplist(PropList)} end,Reply),
    {reply,Reply1,State}.

%% @private
terminate(_Reason, _State) ->
    ok.



%=============================================== Private =====================================

start_crawler_on_node(Node,PropertyList) ->
    start_on_node(Node,PropertyList,start_crawler).

start_session_on_node(Node,PropertyList) ->
    start_on_node(Node,PropertyList,start_session).

start_on_node(Node,PropertyList,FunName) ->
    AtomNode = list_to_atom(Node),
    rpc:call(AtomNode,application,start, [session_manager]),
    CustomizedList = customize(PropertyList),
    io:format("~p\n",[CustomizedList]),
    rpc:call(AtomNode,application,set_env,[session_manager,prop_list,CustomizedList]),
    Response = rpc:call(AtomNode,session_manager_server,FunName,[CustomizedList]),
    serialize_to_proplist(Response).

customize([]) -> [];
customize([{"remote_manager_server_node",V} | PropertyList]) ->
    [{remote_manager_server_node,list_to_atom(V)} | customize(PropertyList)];

customize([{"max_process_count",V} | PropertyList]) ->
    [{max_process_count,element(1,string:to_integer(V))} | customize(PropertyList)];

customize([{"buffer_size",V} | PropertyList]) ->
    [{buffer_size,element(1,string:to_integer(V))} | customize(PropertyList)];

customize([{"trigger_time",V} | PropertyList]) ->
    [{trigger_time,element(1,string:to_integer(V))} | customize(PropertyList)];

customize([{"contact_nodes",V} | PropertyList]) ->
    [{contact_nodes,extract_term_from_string(V)} | customize(PropertyList)];

customize([{"domain_manager_node",V} | PropertyList]) ->
    [{domain_manager_node,list_to_atom(V)} | customize(PropertyList)];

customize([{"init_url", V} | PropertyList]) ->
    [{init_url,V} | customize(PropertyList)];

customize([{"init_urls",V} | PropertyList]) ->
    [{init_urls,extract_term_from_string(V)} | customize(PropertyList)];

customize([{"width", V} | PropertyList]) ->
    [{width,element(1,string:to_integer(V))} | customize(PropertyList)];

customize([{"depth", V} | PropertyList]) ->
    [{depth,element(1,string:to_integer(V))} | customize(PropertyList)];

customize([{"default_validity_time", V} | PropertyList]) ->
    [{default_validity_time,element(1,string:to_integer(V))} | customize(PropertyList)];

customize([{"session_id",V} | PropertyList]) ->
    [{session_id,element(1,string:to_integer(V))} | customize(PropertyList)];

customize([{"default_breadth", V} | PropertyList]) ->
    [{default_breadth,element(1,string:to_integer(V))} | customize(PropertyList)];

customize([{"default_depth", V} | PropertyList]) ->
    [{default_depth,element(1,string:to_integer(V))} | customize(PropertyList)];

customize([{"subdomain_depth", V} | PropertyList]) ->
    [{subdomain_depth,element(1,string:to_integer(V))} | customize(PropertyList)]; 

customize([{"subdomain_breadth", V} | PropertyList]) ->
    [{subdomain_breadth,element(1,string:to_integer(V))} | customize(PropertyList)];

customize([{"subdomain_validity_time", V} | PropertyList]) ->
    [{subdomain_validity_time,element(1,string:to_integer(V))} | customize(PropertyList)].           
    

serialize_to_proplist(List) ->
    try
      lists:map(fun({K,V}) -> {lists:flatten(io_lib:format("~p",[K])), lists:flatten(io_lib:format("~p",[V]))} end,List)
    catch
      _:_ -> []
    end.

extract_term_from_string(Str) ->
    NewStr = case lists:last(Str) of
        46 -> Str;
        _ -> Str ++ [46]
    end,
    {ok,Tokens,_} = erl_scan:string(NewStr),
    {ok,Abs} = erl_parse:parse_exprs(Tokens),
    {value,Val,_} = erl_eval:exprs(Abs, erl_eval:new_bindings()),
    Val.


