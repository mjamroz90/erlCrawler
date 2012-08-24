%% @doc Aplikacja zarzadzajaca calym procesem crawlu na wszystkich wezlach.
%% Na kazdym wezle korzysta ona z aplikacji session_manager do sterowania wyzej wspomnianym procesem.
%% Udostepnia zewnetrznym komponentom napisanym niekoniecznie w erlang'u dostep do crawler'a w oparciu
%% o technologie CORBA (Common Object Request Broker Architecture).
-module(remote_crawl_manager_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @doc Uruchamia aplikacje.
start(_Type, _Args) ->
    Val = case application:get_env(remote_crawl_manager,web_app_controller_url) of
        {ok,Val1} -> Val1;
        undefined -> undefined
    end,
    remote_crawl_manager_sup:start(Val).

%% @doc Zatrzymuje aplikacje.
stop(_State) ->
    ok.