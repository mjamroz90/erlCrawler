%% @doc Modul odpowiedzialny za przetwarzanie z wykorzystaniem wyrazen regularnych.
%% @end

-module(reg).
-behaviour(gen_server).
-export([start_link/0,get_domain/1, stop/0]).
-record(state,{http_pattern, www_pattern, slash_pattern, query_string_pattern}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%======================================API==================================

%% @spec start_link() -> {ok, pid()} | {error, term()}
%% @doc Uruchamia serwer kompilujac odpowiednie wyrazenia regularne.
%% @end
start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% @spec get_domain(Url :: string()) -> string()
%% @doc Dla podanego adresu url zwraca nazwe domeny.
%% @end
get_domain(Url) ->
	gen_server:call(?MODULE, {get_domain, Url}).

%% @spec stop() -> ok
%% @doc Zatrzymuje serwer.
%% @end
stop() ->
	gen_server:cast(?MODULE,stop).


%get_domain(Url) ->
	%Options = [caseless],
	%{ok, HTTP} = re:compile("^http://", Options),
	%{ok, WWW} = re:compile("^www\.", Options),
	%{ok, SLASH} = re:compile("/.*", Options),
	%{ok, QUERY_STRING} = re:compile("\\?.*", Options),
	
	%RetType = [{return, list}],
	%re:replace(re:replace(re:replace(re:replace(Url, HTTP, "", RetType), WWW, "", RetType), SLASH, "", RetType), QUERY_STRING, "", RetType).


%===================================Callbacks==============================
%% @private
init([]) ->	
	Options = [caseless],
	{ok, HTTP} = re:compile("^http://", Options),
	{ok, WWW} = re:compile("^www\.", Options),
	{ok, SLASH} = re:compile("/.*", Options),
	{ok, QUERY_STRING} = re:compile("\\?.*", Options),
	State = #state{http_pattern = HTTP, www_pattern = WWW, slash_pattern = SLASH, query_string_pattern = QUERY_STRING},
	{ok, State}.
	
%% @private		
handle_cast(stop,State) ->
	{stop,"Made to stop",State}.
	
%% @private		
handle_call({get_domain,Url}, _From, State) ->
	RetType = [{return, list}],
	Reply = re:replace(re:replace(re:replace(re:replace(Url, State#state.http_pattern, "", RetType),
			State#state.www_pattern, "", RetType),
			State#state.slash_pattern, "", RetType),
			State#state.query_string_pattern, "", RetType),
	{reply,Reply,State}.
		
%% @private	
handle_info(_Msg,State) ->
	{noreply,State}.
	
%% @private	
terminate(_Reason,_State) ->
	ok.
	
%% @private	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
