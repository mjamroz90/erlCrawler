%% @doc Supervisor nadzorujacy menadzera sesji.

-module(session_manager_sup).
-behaviour(supervisor).
-export([start/0]).
-export([init/1]).

%% @spec start() -> {ok,pid()} | {error,term()}
%% @doc Uruchamia supervisor nadzorujacy menadzera sesji.
start() ->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

%% @private.
init([]) ->
    SessionManServer = {session_manager_server,{session_manager_server,start,[]},
        permanent,brutal_kill,worker,[session_manager_server]},

    RestartStrategy = {one_for_all,100,1},
    {ok,{RestartStrategy,[SessionManServer]}}.

