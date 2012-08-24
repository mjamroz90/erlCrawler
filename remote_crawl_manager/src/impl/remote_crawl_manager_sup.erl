%% @doc Proces nadzorujacy zdalnego menadzera. Nadzorowane procesy to reporting_server, oraz CORBA supervisor.

-module(remote_crawl_manager_sup).
-behaviour(supervisor).
-export([start/1]).
-export([init/1]).

%% @spec start(WebAppCtrlUrl :: string()) -> {ok,Pid :: pid()} | {error, term()}
%% @doc Uruchamia supervisor nadzorujacy zdalnego menadzera.
start(WebAppCtrlUrl) ->
    supervisor:start_link({local,?MODULE},?MODULE,[WebAppCtrlUrl]).

%% @private
init([WebAppCtrlUrl]) ->
     ReportingServer = {reporting_server,{reporting_server,start,[WebAppCtrlUrl]},
        permanent,brutal_kill,worker,[reporting_server]},

    CorbaObjectSup = {corba_object_sup,{corba_object_sup,start,[]},
        permanent,brutal_kill,worker,[corba_object_sup]},
    RestartStrategy = {one_for_all,100,1},
    {ok,{RestartStrategy,[ReportingServer,CorbaObjectSup]}}.
