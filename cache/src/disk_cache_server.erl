-module(disk_cache_server).
-export([start/2,insert/2,lookup/1,update/2,delete/1,change_host/2,stop/0,delete_all/0, pull_urls/1, set_visited/1, set_not_visited/1, 
		get_param/2]).
-record(state,{nodename, port, riakc_pid}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
    
-define(URL_BUCKET,term_to_binary("Urls")).
-define(ID_URL_BUCKET,term_to_binary("IdUrls")).
   
%======================================API==================================
         
start(NodeName,Port) ->
	gen_server:start_link({local,?MODULE},?MODULE,[NodeName,Port],[]).
	
insert(Url,Params) ->
	gen_server:call(?MODULE,{insert,{Url,Params}}).
	
lookup(Url) ->
	gen_server:call(?MODULE,{lookup,Url}).
	
update(Url,Params) ->
	gen_server:call(?MODULE,{update,{Url,Params}}).
	
delete(Url) ->
	gen_server:call(?MODULE,{delete,Url}).

delete_all() ->
	gen_server:call(?MODULE,delete_all,infinity).
	
pull_urls(Count) ->
	gen_server:call(?MODULE, {pull_urls, Count}).
	
set_visited(Url) ->
	gen_server:call(?MODULE, {set_visited, Url}).

set_not_visited(Url) ->
	gen_server:call(?MODULE, {set_not_visited, Url}).
	
change_host(NewNodeName,NewPort) ->
	gen_server:call(?MODULE,{change_host,{NewNodeName,NewPort}}).

get_param(ParamName,ParamList) ->
	get_param1(ParamName,ParamList).
	
stop() ->
	gen_server:cast(?MODULE,stop).
	
%===================================Callbacks==============================

%%byc moze na nowo utworzonym buckecie trzeba będzie ustawić n_val = 1

init([NodeName,Port]) ->	
	case riakc_pb_socket:start_link(NodeName,Port) of
		{ok,Pid} -> 					
					io:format("Parametry: Url_Bucket ~p , Id_Url_Bucket ~p \n",[set_n_val(?URL_BUCKET,1,Pid),set_n_val(?ID_URL_BUCKET,1,Pid)]),
					{ok,#state{riakc_pid = Pid}};
		{error,Reason} -> {stop,Reason}
	end.
		
handle_cast(stop,State) ->
	{stop,"Made to stop",State}.
	
handle_call(delete_all,_From,State = #state{riakc_pid = Pid}) ->
	io:format("Szukam kluczy\n"),
	Result = case riakc_pb_socket:list_keys(Pid,?URL_BUCKET) of		
		{ok,Keys} -> 			
			io:format("Znalazlem klucze\n,dlugosc listy:~p\n",[length(Keys)]),lists:foreach(fun(Key) -> riakc_pb_socket:delete(Pid,?URL_BUCKET,Key) end,Keys),ok;
		Err ->	Err
	end,	
	{reply,Result,State};	
		
handle_call({update,{Url,Params}},_From,State = #state{riakc_pid = Pid}) ->
	{ok,Obj} = riakc_pb_socket:get(Pid,?URL_BUCKET,term_to_binary(Url)),
	Updated_Obj = riakc_obj:update_value(Obj,term_to_binary(Params)),
	Result = riakc_pb_socket:put(Pid,Updated_Obj),
	{reply,Result,State};
	
handle_call({insert,{Url,Params}},_From,State = #state{riakc_pid = Pid}) ->	
	Id = generate_id(),
	NewParams = stick_params({id,Id},Params),
	spawn(fun() -> insert_id_url(Pid,Id,Url) end),
	Object = riakc_obj:new(?URL_BUCKET,term_to_binary(Url),term_to_binary(NewParams)),
	Index = [{term_to_binary("visited_bin"), term_to_binary(no)}],
	Meta = dict:store(<<"index">>, Index, riakc_obj:get_update_metadata(Object)),
	Object2 = riakc_obj:update_metadata(Object, Meta),
	Result = riakc_pb_socket:put(Pid,Object2),
	{reply,Result,State};
		
handle_call({lookup,Url},_From,State = #state{riakc_pid = Pid}) ->
	Result = case riakc_pb_socket:get(Pid,?URL_BUCKET,term_to_binary(Url)) of
		{ok,Object} -> 
			binary_to_term(riakc_obj:get_value(Object));
			
		{error,_} -> not_found
	end,
	{reply,Result,State};	
 
	
handle_call({delete,Url},_From,State = #state{riakc_pid = Pid}) ->
	Result = riakc_pb_socket:delete(Pid,?URL_BUCKET,term_to_binary(Url)),
	{reply,Result,State}; 

	 
handle_call({change_host,{NewNodeName,NewPort}},_From,State = #state{riakc_pid=Pid}) ->
	Result = case riakc_pb_socket:start_link(NewNodeName,NewPort) of
		{ok,NewPid} -> riakc_pb_socket:stop(Pid),ok;
		
		{error,Reason} -> NewPid = Pid,{error,Reason}
	end,
	{reply,Result,State#state{nodename=NewNodeName,port=NewPort,riakc_pid=NewPid}};
	
handle_call({pull_urls, Count}, _From, State = #state{riakc_pid = Pid}) ->
	{reply, pull_urls(Pid, ?URL_BUCKET, Count), State};
	
handle_call({set_visited, Url}, _From, State = #state{riakc_pid = Pid}) ->
	case riakc_pb_socket:get(Pid,?URL_BUCKET,term_to_binary(Url)) of
		{ok,Object} -> 
			Index = [{term_to_binary("visited_bin"), term_to_binary(yes)}],
			Meta = dict:store(<<"index">>, Index, riakc_obj:get_update_metadata(Object)),
			Object2 = riakc_obj:update_metadata(Object, Meta),
			Result = riakc_pb_socket:put(Pid,Object2);
		{error,Term} -> Result = {error, Term}
	end,
	{reply, Result, State};
	
handle_call({set_not_visited, Url}, _From, State = #state{riakc_pid = Pid}) ->
	case riakc_pb_socket:get(Pid,?URL_BUCKET,term_to_binary(Url)) of
		{ok,Object} -> 
			Index = [{term_to_binary("visited_bin"), term_to_binary(no)}],
			Meta = dict:store(<<"index">>, Index, riakc_obj:get_update_metadata(Object)),
			Object2 = riakc_obj:update_metadata(Object, Meta),
			Result = riakc_pb_socket:put(Pid,Object2);
		{error,Term} -> Result = {error, Term}
	end,
	{reply, Result, State}.
	
handle_info(_Msg,State) ->
	{noreply,State}.
	
terminate(_Reason,#state{riakc_pid = Pid}) ->
	riakc_pb_socket:stop(Pid),
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	


%=======================================Internal=======================================

set_n_val(BucketName,Val,Pid) ->
	{ok,Buckets} = riakc_pb_socket:list_buckets(Pid),
	case lists:member(BucketName,Buckets) of 
		false -> riakc_pb_socket:set_bucket(Pid,BucketName,[{n_val,Val}]);
		_ -> ok
	end,
	{ok,Prop} = riakc_pb_socket:get_bucket(Pid,BucketName),
	Prop.

get_param1(ParamName,ParamList) ->
	case lists:keyfind(ParamName,1,ParamList) of
		false -> not_found;
		{_,Value} -> Value
	end.

insert_id_url(Pid,Id,Url) ->
	Object = riakc_obj:new(?ID_URL_BUCKET,term_to_binary(Id),term_to_binary(Url)),	
	riakc_pb_socket:put(Pid,Object).

stick_params(NewTuple,Params) ->	
	NewParams = lists:keydelete(element(1,NewTuple),1,Params),
	[NewTuple | NewParams].

generate_id() ->
	{Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro - 1339670326906632.
	% 1339670326906632 - magic Value
% returns list up to Count urls which have secondary index visited_bin=no
pull_urls(Pid, Bucket, Count) ->
	case get_index_count(Pid, Bucket, term_to_binary("visited_bin"), term_to_binary(no), Count) of
		{ok, Result} -> Result;
		{error, {timeout, Result}} -> Result;
		Error -> Error
	end.
		

% runs map reduce to get Count of keys with given secondary index value
get_index_count(Pid, Bucket, Index, Key, Count) ->
	Input = {index, Bucket, Index, Key},
    IdentityQuery = [{reduce,
                      {modfun, riak_kv_mapreduce, reduce_identity},
                      %[{reduce_phase_only_1, true}],
                      [],
                      true}],
      
    Receiver = self(),
    Timeout = 1000,
    
    case riakc_pb_socket:mapred_stream(Pid, Input, IdentityQuery, Receiver) of
        {ok, ReqId} ->
            wait_for_mapred(ReqId, Timeout, Count);
        Error ->
            Error
    end.
    
    
wait_for_mapred(ReqId, Timeout, Count) ->
    wait_for_mapred(ReqId,Timeout,[], Count).
wait_for_mapred(_ReqId, _Timeout, Acc, 0) ->
	{ok, Acc};
wait_for_mapred(ReqId, Timeout, Acc, Count) ->
    receive
        {ReqId, done} -> {ok, Acc};
        {ReqId, {mapred,_Phase,Res}} ->
			[[_H|[H2|_T]]] = Res,
            wait_for_mapred(ReqId,Timeout,[binary_to_term(H2) | Acc], Count-1);
        {ReqId, {error, Reason}} -> {error, Reason}
    after Timeout ->
            {error, {timeout, Acc}}
    end.

	
