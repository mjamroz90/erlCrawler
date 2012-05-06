-module(ram_cache_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
		 
-export([start/1,insert/2,lookup/1,delete/1,stop/0,get_cache_size/0,delete_all/0]).
-record(state,{current_item_number,max_item_number,ram_table}).

%================================API===========================================
start(Max_Item_Number) ->
	gen_server:start_link({local,?MODULE},?MODULE,[Max_Item_Number],[]).

insert(Url,Params) ->
	gen_server:cast(?MODULE,{insert,{Url,Params}}).

lookup(Url) ->
	gen_server:call(?MODULE,{lookup,Url}).

delete(Url) ->
	gen_server:cast(?MODULE,{delete,Url}).	
	
delete_all() ->
	gen_server:cast(?MODULE,delete_all).
		
get_cache_size() ->
	gen_server:call(?MODULE,get_cache_size).
	
stop() ->
	gen_server:cast(?MODULE,stop).
	
%===============================Callbacks======================================

init([Max_Item_Number]) ->
	State = #state{current_item_number=0,max_item_number=Max_Item_Number,ram_table=ram_cache:new(ram_table)},
	{ok,State}.
	
%=trzeba dopisac wstawianie na dysk
handle_cast({insert,{Url,Params}},State = #state{max_item_number=Max,current_item_number=CurrNum,ram_table=Ram_Table}) ->
	Curr_Num = case CurrNum == Max of
		true -> ram_cache:remove_random(Ram_Table),
				ram_cache:insert(Ram_Table,Url,Params),
				CurrNum;
		false -> ram_cache:insert(Ram_Table,Url,Params),CurrNum+1
	end,
	{noreply,State#state{current_item_number=Curr_Num}};

handle_cast(delete_all, State = #state{ram_table=Ram_Table}) ->
	ram_cache:delete_all(Ram_Table),
	{noreply,State#state{current_item_number = 0}};
	
handle_cast({delete,Url},State = #state{current_item_number=CurrNum,ram_table=Ram_Table}) ->
	Curr_Num = 
	case ram_cache:exists(Ram_Table,Url) of 
		true -> ram_cache:delete(Ram_Table,Url),
				CurrNum-1;
		false -> CurrNum
	end,
	{noreply,State#state{current_item_number=Curr_Num}};

handle_cast(stop,State) ->
	{stop,"Made to stop",State}.

handle_call({lookup,Url},_From,State=#state{ram_table=Ram_Table}) ->
	Result = case ram_cache:lookup(Ram_Table,Url) of
		[] -> not_found;
		Value -> Value
	end,
	{reply,Result,State};
	
handle_call(get_cache_size,_From,State=#state{current_item_number=CurrNum}) ->	
	{reply,CurrNum,State}.
	
handle_info(_Msg,State) ->
	{noreply,State}.

terminate(_Reason,_State = #state{ram_table=Ram_Table}) ->
	ram_cache:remove(Ram_Table),
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
