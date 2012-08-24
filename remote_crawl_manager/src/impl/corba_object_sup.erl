%% @doc Proces nadzorujacy serwer - obiekt CORBA. Wykorzystuje on wzorzec supervisor_bridge.

-module(corba_object_sup).
-behaviour(supervisor_bridge).
-export([start/0]).
-export([init/1, terminate/2]).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/COSS/CosNaming/lname.hrl").

%% @spec start() -> {ok,Pid :: pid()} | {error, term()}
%% @doc Uruchamia supervisor'a CORBA.
start() ->
    supervisor_bridge:start_link({local,?MODULE},?MODULE,[]).

%%==============================================CallBack=====================================

%% @private
init([]) ->
    mnesia:create_schema([]),
    try
        orber:install([])
    catch
        _:_ -> void
    end,
    orber:start(),
    try
        oe_RemoteManager:oe_register()
    catch
        _:_ -> void
    end,
    {ok,Pid,ObjRef} = 'RemoteManager_RemoteManagerServer':oe_create_link([],[{sup_child,true}]),
    NS = corba:resolve_initial_references("NameService"),
    NC = lname_component:set_id(lname_component:create(), "RemoteCrawlServer"),
    N = lname:insert_component(lname:create(), 1, NC),
    'CosNaming_NamingContext':rebind(NS, N, ObjRef),
    {ok,Pid,ObjRef}.

%% @private
terminate(_Reason,ObjRef) ->
    corba:dispose(ObjRef),
    orber:stop(),
    ok.



