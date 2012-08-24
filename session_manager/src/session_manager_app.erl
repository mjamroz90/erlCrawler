%% @doc Aplikacja wykonujaca podstawowe operacje startowania/zatrzymaywania crawler'a, oraz sesji crawlu na danym wezle.

-module(session_manager_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @doc Startuje aplikacje session_manager
start(_Type, _Args) ->
    session_manager_sup:start().

%% @doc Zatrzymuje aplikacje session_manager.
stop(_State) ->
    ok.