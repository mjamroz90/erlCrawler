%% @doc Supervisor nadzorujacy procesy przetwarzajace, uruchamia na zadanie kolejne identyczne procesy.
%% @end

-module(processing_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/2, start_child/3, count_children/0, init/1]).

%% @spec start_link() -> ok | {error, term()}
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec start_child(Id :: term(), Url :: string()) -> {ok, Pid :: pid()} | {error, term()}
%% @doc Uruchamia proces przetwarzajacy z parametrami:
%% <ul>
%% <li> Id - identyfikator strony</li>
%% <li> Url - adres strony</li>
%% </ul>
%% @end
start_child(Id, Url) ->
	supervisor:start_child(?MODULE, [Id, Url]).
	
%% @spec start_child(Id :: term(), Url :: string(), Source :: binary()) -> {ok, Pid :: pid()} | {error, term()}
%% @doc Uruchamia proces przetwarzajacy z parametrami:
%% <ul>
%% <li> Id - identyfikator strony</li>
%% <li> Url - adres strony</li>
%% <li> Source - pobrane zrodlo strony</li>
%% </ul>
%% @end
start_child(Id, Url, Source) ->
	supervisor:start_child(?MODULE, [Id, Url, Source]).

%% @spec count_children() -> integer()
%% @doc Zwraca liczbe aktualnie pracujacych procesow przetwarzajacych
%% @end
count_children() ->
	supervisor:count_children(?MODULE).

%% @private	
init([]) ->
	Element = {url_processing, {url_processing, start_link, []},
		temporary, brutal_kill, worker, [url_processing]},
	
	RestartStrategy = {simple_one_for_one, 0, 10},
	
	{ok, {RestartStrategy, [Element]}}.
	
