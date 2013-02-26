-module(download_worker).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start/2, download_content/3]).
-record(state,{connection_timeout,idle_time,self_pid,port}).

-define(CORRECT_HTTP_STATUS,[200,202,206]).

%% ============================ API ===================================

%%TODO pamietac o uruchomieniu inets:start()

start(ConnTimeout,IdleTime) ->
	gen_server:start(?MODULE,[ConnTimeout,IdleTime],[]).

%% zwraca {download_error,Url} | {Url,{PlainText,[link()]}}
download_content(WorkerPid,Url,Pid) ->
	gen_server:cast(WorkerPid,{download_content,{Url,Pid}}).

%% ============================ Callbacks =============================

init([ConnTimeout,IdleTime]) ->
	{Res,Port} = case erl_ddll:load("priv","parse_html_driver") of	
		ok -> {ok, open_port({spawn,"parse_html_driver"},[binary])};
		_Other -> {stop,stop}
	end,		
	{Res,#state{connection_timeout=ConnTimeout, idle_time=IdleTime, self_pid=self(), port = Port}, IdleTime}.

handle_info(timeout,State) ->	
	{stop,"Idle timeout exceeded",State}.

%% TODO inkrementacja mb_counter w pid_storage_server - zadanie dla url_downloader
handle_cast({download_content,{Url,Pid}},State = #state{connection_timeout=ConnTimeout,idle_time=IdleTime, port = Port}) ->	
	Response = generate_response(Url,ConnTimeout,Port),
	send_reponse_to_client(Pid,Response),
	{noreply,State,IdleTime}.

handle_call(_Req,_From,State) ->
	{reply,ok,State}.

terminate(_Reason, #state{self_pid = SelfPid}) ->	
	%% trzeba sie wyrejestrowac z pid_storage_server	
	pid_storage_server:delete(SelfPid),
	ok.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.


%% =========================== Private ================================

generate_response(Url,ConnTimeout,Port) ->
	Response = case download_content_http(Url,ConnTimeout) of
		{ok,Content} -> {Url,extract_urls(Content,Port,Url)};
		Error -> Error
	end,
	Response.

send_reponse_to_client(Pid,Resp) ->
	gen_server:reply(Pid,Resp),
	pid_storage_server:dec_mb_counter(self()).

%% wysyla httpRequest i zwraca tresc
download_content_http(Url,ConnTimeout) ->
	case httpc:request(get,{Url,[]},[{timeout,ConnTimeout}],[]) of 
		{ok,{{_Ver,Status,_RPhrase},_Headers,Body}} -> 
			case contains(?CORRECT_HTTP_STATUS,Status) of 
				true -> {ok,Body};
				false -> {download_error,Url}
			end;			
		{error,_Reason} -> {download_error,Url}
	end.
	
%%TODO wydobywa adresy url, zwraca {whole_text,[link]} | {error,text}
extract_urls(HttpContent,Port,Url) ->
	Data = {Url,HttpContent},
	Port ! {self(),{command, term_to_binary(Data)}},	
	Repl = receive
		{Port,{data,Reply}} ->			
			binary_to_term(Reply)			
	end,
	Repl.

contains([],Elem) ->
	false;
contains([Elem|T],Elem) ->
	true;
contains([_|T],Elem) ->
	contains(T,Elem).			