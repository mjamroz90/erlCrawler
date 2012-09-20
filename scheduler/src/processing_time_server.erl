-module(processing_time_server).

-behaviour(gen_server).

-record(state,{counter, total_time}).

-export([start/0,report/1,get_mean_time/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).
	
report(Time) ->
	gen_server:call(?MODULE, {report, Time}).
	
get_mean_time() ->
	gen_server:call(?MODULE,get_mean_time).
	
stop() ->
	gen_server:cast(?MODULE,stop).

%===================================Callbacks==============================

%% @private
init([]) ->	
	State = #state{counter = 0, total_time = 0},
	{ok, State}.
	
%% @private		
handle_cast(stop,State) ->
	{stop,"Made to stop",State}.

%% @private	
handle_call(get_mean_time, _From, State = #state{counter = Counter, total_time = TotalTime}) ->
	Reply = case Counter of
		0 -> 0;
		N -> TotalTime/N
	end,
	NewState = State#state{counter = 0, total_time = 0},
	{reply, Reply, NewState};
	
handle_call({report, Time}, _From, State = #state{counter = Counter, total_time = TotalTime}) ->
	NewState = State#state{counter = Counter + 1, total_time = TotalTime + Time},
	{reply, ok, NewState}.
	
%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
