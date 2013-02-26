-module(pid_storage_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start/0,stop/0,insert/1,get_size/0,fetch_pid/0,
	inc_mb_counter/1,dec_mb_counter/1,delete/1]).
-behaviour(gen_server).
-record(state,{}).
-record(pid_to_mbsize,{pid,mbsize}).
-define(TAB_NAME,pid_to_mbsize).

-include_lib("stdlib/include/qlc.hrl").

%% ================================ API ===========================================

start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop() ->
	gen_server:cast(?MODULE,stop).

insert(Pid) ->
	gen_server:call(?MODULE,{insert,Pid}).

get_size() ->
	gen_server:call(?MODULE,get_size).

fetch_pid() ->
	gen_server:call(?MODULE,fetch_pid).

inc_mb_counter(Pid) ->
	gen_server:call(?MODULE,{inc_mb_counter,Pid}).

dec_mb_counter(Pid) ->
	gen_server:call(?MODULE,{dec_mb_counter,Pid}).		

delete(Pid) ->
	gen_server:call(?MODULE,{delete,Pid}).

%% ================================ Callbacks ======================================	


init([]) ->
	mnesia:start(),
	mnesia:create_table(?TAB_NAME,[{ram_copies,[node()]},
		{attributes,record_info(fields,pid_to_mbsize)},{index,[mbsize]}]),
	{ok,#state{}}.

handle_call({insert,Pid},_From,State) ->
	F = fun() ->
			mnesia:write(#pid_to_mbsize{pid=Pid,mbsize=0}) end,
	mnesia:transaction(F),	
	{reply,ok,State};

handle_call(get_size,_From,State) ->
	{reply,mnesia:table_info(?TAB_NAME,size),State};

handle_call(fetch_pid,_From,State) ->
	F = fun() -> Query = qlc:q([P || P <- mnesia:table(?TAB_NAME)]),
				Query1 = qlc:sort(Query,{order,fun(P_to_mbsize1,P_to_mbsize2) -> 
				P_to_mbsize1#pid_to_mbsize.mbsize < P_to_mbsize2#pid_to_mbsize.mbsize end}),
				C = qlc:cursor(Query1),
				[{pid_to_mbsize,Result_pid,_Count}] = qlc:next_answers(C,1),
				Result_pid end,
	{atomic,Result} = mnesia:transaction(F),				
	{reply,Result,State};

handle_call({inc_mb_counter,Pid},_From,State) ->	
	{reply,change_counter(Pid,1),State};

handle_call({dec_mb_counter,Pid},_From,State) ->	
	{reply,change_counter(Pid,-1),State};

handle_call({delete,Pid},_From,State) ->
	F = fun() ->
		mnesia:delete({?TAB_NAME,Pid}) end,
	Res = mnesia:transaction(F),
	{reply,Res,State}.			

handle_cast(stop,State) ->
	{stop,"Made to stop",State};

handle_cast(_Req,State) ->
	{noreply,State}.

handle_info(_Req,State) ->
	{noreply,State}.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.

terminate(_Reason,_State) ->	
	ok.	

%% ====================================== private ===========================================

change_counter(Pid,How) ->
	F = fun() ->
		[{pid_to_mbsize,Pid,Counter}] = mnesia:read(?TAB_NAME,Pid),
		mnesia:write(#pid_to_mbsize{pid=Pid,mbsize=Counter+How}) end,
	{atomic,Res} = mnesia:transaction(F),
	Res.