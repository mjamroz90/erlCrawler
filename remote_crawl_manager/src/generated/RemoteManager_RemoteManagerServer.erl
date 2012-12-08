%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: RemoteManager_RemoteManagerServer
%% Source: /home/michal/nauka/erlang/erlCrawler/remote_crawl_manager/src/idl/RemoteManager.idl
%% IC vsn: 4.2.30
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('RemoteManager_RemoteManagerServer').
-ic_compiled("4_2_30").


%% Interface functions
-export([startCrawlerOnNode/3, startCrawlerOnNode/4, startCrawlerOnNodes/2]).
-export([startCrawlerOnNodes/3, startSessionOnNode/3, startSessionOnNode/4]).
-export([startSessionOnNodes/2, startSessionOnNodes/3, pingNode/2]).
-export([pingNode/3, pingApp/3, pingApp/4]).
-export([stopSessionOnNode/2, stopSessionOnNode/3, stopSessionOnNodes/2]).
-export([stopSessionOnNodes/3, stopCrawlerOnNode/2, stopCrawlerOnNode/3]).
-export([stopCrawlerOnNodes/2, stopCrawlerOnNodes/3]).

%% Type identification function
-export([typeID/0]).

%% Used to start server
-export([oe_create/0, oe_create_link/0, oe_create/1]).
-export([oe_create_link/1, oe_create/2, oe_create_link/2]).

%% TypeCode Functions and inheritance
-export([oe_tc/1, oe_is_a/1, oe_get_interface/0]).

%% gen server export stuff
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3]).

-include_lib("orber/include/corba.hrl").


%%------------------------------------------------------------
%%
%% Object interface functions.
%%
%%------------------------------------------------------------



%%%% Operation: startCrawlerOnNode
%% 
%%   Returns: RetVal
%%
startCrawlerOnNode(OE_THIS, NodeName, PropertyList) ->
    corba:call(OE_THIS, startCrawlerOnNode, [NodeName, PropertyList], ?MODULE).

startCrawlerOnNode(OE_THIS, OE_Options, NodeName, PropertyList) ->
    corba:call(OE_THIS, startCrawlerOnNode, [NodeName, PropertyList], ?MODULE, OE_Options).

%%%% Operation: startCrawlerOnNodes
%% 
%%   Returns: RetVal
%%
startCrawlerOnNodes(OE_THIS, NodeProperties) ->
    corba:call(OE_THIS, startCrawlerOnNodes, [NodeProperties], ?MODULE).

startCrawlerOnNodes(OE_THIS, OE_Options, NodeProperties) ->
    corba:call(OE_THIS, startCrawlerOnNodes, [NodeProperties], ?MODULE, OE_Options).

%%%% Operation: startSessionOnNode
%% 
%%   Returns: RetVal
%%
startSessionOnNode(OE_THIS, NodeName, PropList) ->
    corba:call(OE_THIS, startSessionOnNode, [NodeName, PropList], ?MODULE).

startSessionOnNode(OE_THIS, OE_Options, NodeName, PropList) ->
    corba:call(OE_THIS, startSessionOnNode, [NodeName, PropList], ?MODULE, OE_Options).

%%%% Operation: startSessionOnNodes
%% 
%%   Returns: RetVal
%%
startSessionOnNodes(OE_THIS, NodeProperties) ->
    corba:call(OE_THIS, startSessionOnNodes, [NodeProperties], ?MODULE).

startSessionOnNodes(OE_THIS, OE_Options, NodeProperties) ->
    corba:call(OE_THIS, startSessionOnNodes, [NodeProperties], ?MODULE, OE_Options).

%%%% Operation: pingNode
%% 
%%   Returns: RetVal, Message
%%
pingNode(OE_THIS, NodeName) ->
    corba:call(OE_THIS, pingNode, [NodeName], ?MODULE).

pingNode(OE_THIS, OE_Options, NodeName) ->
    corba:call(OE_THIS, pingNode, [NodeName], ?MODULE, OE_Options).

%%%% Operation: pingApp
%% 
%%   Returns: RetVal, Message
%%
pingApp(OE_THIS, NodeName, AppName) ->
    corba:call(OE_THIS, pingApp, [NodeName, AppName], ?MODULE).

pingApp(OE_THIS, OE_Options, NodeName, AppName) ->
    corba:call(OE_THIS, pingApp, [NodeName, AppName], ?MODULE, OE_Options).

%%%% Operation: stopSessionOnNode
%% 
%%   Returns: RetVal
%%
stopSessionOnNode(OE_THIS, NodeName) ->
    corba:call(OE_THIS, stopSessionOnNode, [NodeName], ?MODULE).

stopSessionOnNode(OE_THIS, OE_Options, NodeName) ->
    corba:call(OE_THIS, stopSessionOnNode, [NodeName], ?MODULE, OE_Options).

%%%% Operation: stopSessionOnNodes
%% 
%%   Returns: RetVal
%%
stopSessionOnNodes(OE_THIS, Nodes) ->
    corba:call(OE_THIS, stopSessionOnNodes, [Nodes], ?MODULE).

stopSessionOnNodes(OE_THIS, OE_Options, Nodes) ->
    corba:call(OE_THIS, stopSessionOnNodes, [Nodes], ?MODULE, OE_Options).

%%%% Operation: stopCrawlerOnNode
%% 
%%   Returns: RetVal
%%
stopCrawlerOnNode(OE_THIS, NodeName) ->
    corba:call(OE_THIS, stopCrawlerOnNode, [NodeName], ?MODULE).

stopCrawlerOnNode(OE_THIS, OE_Options, NodeName) ->
    corba:call(OE_THIS, stopCrawlerOnNode, [NodeName], ?MODULE, OE_Options).

%%%% Operation: stopCrawlerOnNodes
%% 
%%   Returns: RetVal
%%
stopCrawlerOnNodes(OE_THIS, Nodes) ->
    corba:call(OE_THIS, stopCrawlerOnNodes, [Nodes], ?MODULE).

stopCrawlerOnNodes(OE_THIS, OE_Options, Nodes) ->
    corba:call(OE_THIS, stopCrawlerOnNodes, [Nodes], ?MODULE, OE_Options).

%%------------------------------------------------------------
%%
%% Inherited Interfaces
%%
%%------------------------------------------------------------
oe_is_a("IDL:RemoteManager/RemoteManagerServer:1.0") -> true;
oe_is_a(_) -> false.

%%------------------------------------------------------------
%%
%% Interface TypeCode
%%
%%------------------------------------------------------------
oe_tc(startCrawlerOnNode) -> 
	{{tk_sequence,{tk_array,{tk_string,0},2},0},
         [{tk_string,0},{tk_sequence,{tk_array,{tk_string,0},2},0}],
         []};
oe_tc(startCrawlerOnNodes) -> 
	{{tk_sequence,{tk_struct,"IDL:RemoteManager/nodeProperty:1.0",
                                 "nodeProperty",'RemoteManager_nodeProperty'},
                      0},
         [{tk_sequence,{tk_struct,"IDL:RemoteManager/nodeProperty:1.0",
                                  "nodeProperty",'RemoteManager_nodeProperty'},
                       0}],
         []};
oe_tc(startSessionOnNode) -> 
	{{tk_sequence,{tk_array,{tk_string,0},2},0},
         [{tk_string,0},{tk_sequence,{tk_array,{tk_string,0},2},0}],
         []};
oe_tc(startSessionOnNodes) -> 
	{{tk_sequence,{tk_struct,"IDL:RemoteManager/nodeProperty:1.0",
                                 "nodeProperty",'RemoteManager_nodeProperty'},
                      0},
         [{tk_sequence,{tk_struct,"IDL:RemoteManager/nodeProperty:1.0",
                                  "nodeProperty",'RemoteManager_nodeProperty'},
                       0}],
         []};
oe_tc(pingNode) -> 
	{tk_boolean,[{tk_string,0}],[{tk_string,0}]};
oe_tc(pingApp) -> 
	{tk_boolean,[{tk_string,0},{tk_string,0}],[{tk_string,0}]};
oe_tc(stopSessionOnNode) -> 
	{{tk_sequence,{tk_array,{tk_string,0},2},0},[{tk_string,0}],[]};
oe_tc(stopSessionOnNodes) -> 
	{{tk_sequence,{tk_struct,"IDL:RemoteManager/nodeProperty:1.0",
                                 "nodeProperty",'RemoteManager_nodeProperty'},
                      0},
         [{tk_sequence,{tk_string,0},0}],
         []};
oe_tc(stopCrawlerOnNode) -> 
	{{tk_sequence,{tk_array,{tk_string,0},2},0},[{tk_string,0}],[]};
oe_tc(stopCrawlerOnNodes) -> 
	{{tk_sequence,{tk_struct,"IDL:RemoteManager/nodeProperty:1.0",
                                 "nodeProperty",'RemoteManager_nodeProperty'},
                      0},
         [{tk_sequence,{tk_string,0},0}],
         []};
oe_tc(_) -> undefined.

oe_get_interface() -> 
	[{"stopCrawlerOnNodes", oe_tc(stopCrawlerOnNodes)},
	{"stopCrawlerOnNode", oe_tc(stopCrawlerOnNode)},
	{"stopSessionOnNodes", oe_tc(stopSessionOnNodes)},
	{"stopSessionOnNode", oe_tc(stopSessionOnNode)},
	{"pingApp", oe_tc(pingApp)},
	{"pingNode", oe_tc(pingNode)},
	{"startSessionOnNodes", oe_tc(startSessionOnNodes)},
	{"startSessionOnNode", oe_tc(startSessionOnNode)},
	{"startCrawlerOnNodes", oe_tc(startCrawlerOnNodes)},
	{"startCrawlerOnNode", oe_tc(startCrawlerOnNode)}].




%%------------------------------------------------------------
%%
%% Object server implementation.
%%
%%------------------------------------------------------------


%%------------------------------------------------------------
%%
%% Function for fetching the interface type ID.
%%
%%------------------------------------------------------------

typeID() ->
    "IDL:RemoteManager/RemoteManagerServer:1.0".


%%------------------------------------------------------------
%%
%% Object creation functions.
%%
%%------------------------------------------------------------

oe_create() ->
    corba:create(?MODULE, "IDL:RemoteManager/RemoteManagerServer:1.0").

oe_create_link() ->
    corba:create_link(?MODULE, "IDL:RemoteManager/RemoteManagerServer:1.0").

oe_create(Env) ->
    corba:create(?MODULE, "IDL:RemoteManager/RemoteManagerServer:1.0", Env).

oe_create_link(Env) ->
    corba:create_link(?MODULE, "IDL:RemoteManager/RemoteManagerServer:1.0", Env).

oe_create(Env, RegName) ->
    corba:create(?MODULE, "IDL:RemoteManager/RemoteManagerServer:1.0", Env, RegName).

oe_create_link(Env, RegName) ->
    corba:create_link(?MODULE, "IDL:RemoteManager/RemoteManagerServer:1.0", Env, RegName).

%%------------------------------------------------------------
%%
%% Init & terminate functions.
%%
%%------------------------------------------------------------

init(Env) ->
%% Call to implementation init
    corba:handle_init('RemoteManager_RemoteManagerServer_impl', Env).

terminate(Reason, State) ->
    corba:handle_terminate('RemoteManager_RemoteManagerServer_impl', Reason, State).


%%%% Operation: startCrawlerOnNode
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, startCrawlerOnNode, [NodeName, PropertyList]}, _, OE_State) ->
  corba:handle_call('RemoteManager_RemoteManagerServer_impl', startCrawlerOnNode, [NodeName, PropertyList], OE_State, OE_Context, false, false);

%%%% Operation: startCrawlerOnNodes
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, startCrawlerOnNodes, [NodeProperties]}, _, OE_State) ->
  corba:handle_call('RemoteManager_RemoteManagerServer_impl', startCrawlerOnNodes, [NodeProperties], OE_State, OE_Context, false, false);

%%%% Operation: startSessionOnNode
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, startSessionOnNode, [NodeName, PropList]}, _, OE_State) ->
  corba:handle_call('RemoteManager_RemoteManagerServer_impl', startSessionOnNode, [NodeName, PropList], OE_State, OE_Context, false, false);

%%%% Operation: startSessionOnNodes
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, startSessionOnNodes, [NodeProperties]}, _, OE_State) ->
  corba:handle_call('RemoteManager_RemoteManagerServer_impl', startSessionOnNodes, [NodeProperties], OE_State, OE_Context, false, false);

%%%% Operation: pingNode
%% 
%%   Returns: RetVal, Message
%%
handle_call({_, OE_Context, pingNode, [NodeName]}, _, OE_State) ->
  corba:handle_call('RemoteManager_RemoteManagerServer_impl', pingNode, [NodeName], OE_State, OE_Context, false, false);

%%%% Operation: pingApp
%% 
%%   Returns: RetVal, Message
%%
handle_call({_, OE_Context, pingApp, [NodeName, AppName]}, _, OE_State) ->
  corba:handle_call('RemoteManager_RemoteManagerServer_impl', pingApp, [NodeName, AppName], OE_State, OE_Context, false, false);

%%%% Operation: stopSessionOnNode
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, stopSessionOnNode, [NodeName]}, _, OE_State) ->
  corba:handle_call('RemoteManager_RemoteManagerServer_impl', stopSessionOnNode, [NodeName], OE_State, OE_Context, false, false);

%%%% Operation: stopSessionOnNodes
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, stopSessionOnNodes, [Nodes]}, _, OE_State) ->
  corba:handle_call('RemoteManager_RemoteManagerServer_impl', stopSessionOnNodes, [Nodes], OE_State, OE_Context, false, false);

%%%% Operation: stopCrawlerOnNode
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, stopCrawlerOnNode, [NodeName]}, _, OE_State) ->
  corba:handle_call('RemoteManager_RemoteManagerServer_impl', stopCrawlerOnNode, [NodeName], OE_State, OE_Context, false, false);

%%%% Operation: stopCrawlerOnNodes
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, stopCrawlerOnNodes, [Nodes]}, _, OE_State) ->
  corba:handle_call('RemoteManager_RemoteManagerServer_impl', stopCrawlerOnNodes, [Nodes], OE_State, OE_Context, false, false);



%%%% Standard gen_server call handle
%%
handle_call(stop, _, State) ->
    {stop, normal, ok, State};

handle_call(_, _, State) ->
    {reply, catch corba:raise(#'BAD_OPERATION'{minor=1163001857, completion_status='COMPLETED_NO'}), State}.


%%%% Standard gen_server cast handle
%%
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.


%%%% Standard gen_server handles
%%
handle_info(_, State) ->
    {noreply, State}.


code_change(OldVsn, State, Extra) ->
    corba:handle_code_change('RemoteManager_RemoteManagerServer_impl', OldVsn, State, Extra).

