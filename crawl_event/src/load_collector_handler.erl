%% @doc Modul stanowiacy callback dla wzorca gen_event. Jest informowany przez Event Manager'a kiedy zostanie zaroportowane obciazenie wezla.
%% Przekazuje to obciazenie wtedy do centralnego serwera zbierajacego obciazenie i robiacego z tej informacji uzytek.
%% @end 

-module(load_collector_handler).
-behaviour(gen_event).
-export([init/1,handle_event/2,code_change/3,terminate/2,handle_info/2,handle_call/2]).
-define(LOAD_COLLECTOR_SERVER,load_collector_server).

%% @type proplist() = [{Key :: term(), Value :: term()}]
%% @type state() = node()

%% @spec init([ NodeToReport :: node()]) -> {ok,NodeToReport :: node()}
%% @doc Jest wywolywana w czasie rejestracji handlera, argumentem jest wezel, na ktorym znajduje sie serwer zbierajacy obciazenie. 
%% Do niego bedzie raportowane obciazenie.
init([NodeToReport]) ->
	{ok,NodeToReport}.

%% 	@spec handle_event({report_load,{Node :: node(), Load :: proplist()}},NodeToReport :: state()) -> {ok,NodeToReport :: state()}
%%  @doc Przechwytuje zdarzenie raportowania obciazenia, wysyla do serwera zbierajacgo te informacje.
handle_event({report_load,{Node,Load}},NodeToReport) ->
	case NodeToReport =:= node() of
		true ->  ?LOAD_COLLECTOR_SERVER:report_load(NodeToReport,Load);
		false -> gen_server:cast({?LOAD_COLLECTOR_SERVER,NodeToReport},{report_load,{Node,Load}})
	end,
	{ok,NodeToReport};

handle_event(_Msg,NodeToReport) ->
    {ok,NodeToReport}.

%% @private
handle_call(_Req,State) ->
	{ok,ok,State}.

%% @private
handle_info(_Info,State) ->
	{ok,State}.

%% @private	
terminate(_Args,_State) ->	
	ok.

%% @private	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
