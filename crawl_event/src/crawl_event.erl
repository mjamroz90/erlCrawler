%% @doc Serwer(tzw. Event Manager) odbierajacy roznorakie zdarzenia zachodzace w systemie. 
%% Przechowuje on liste zarejestrowanych handlerow, ktore sa wywolywane w przypadku zajscia zdarzenia.
%% @end

-module(crawl_event).
-export([start/0,add_handler/2,delete_handler/2,report_load/1]).

%% @type proplist() = [{Key :: term(), Value :: term()}]

%% @spec start() -> ok | {error, term()}
%% @doc Startuje Event Manager'a.
start() ->
	gen_event:start_link({local,?MODULE}).

%% @spec add_handler(Handler :: module(), Args :: term()) -> ok | {'EXIT', term()}
%% @doc Dodajemy handler do Event Manager'a.
add_handler(Handler,Args) ->
	gen_event:add_handler(?MODULE,Handler,Args).
	
%% @spec delete_handler(Handler :: module(), Args :: term()) -> ok
%% @doc Usuwamy handler z Event Manager'a.
delete_handler(Handler,Args) ->
	gen_event:delete_handler(?MODULE,Handler,Args).
	
%% @spec report_load(Load :: proplist()) ->	ok
%% @doc Reportuje obciazenie systemu do Event Manager'a.
report_load(Load) ->
	gen_event:notify(?MODULE,{report_load,{node(),Load}}).
	
