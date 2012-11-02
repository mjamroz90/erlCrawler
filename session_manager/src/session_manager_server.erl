%% @doc Serwer, ktory na zadanie inicjalizuje calego crawler-a tzn. uruchamia wszystkie potrzebne aplikacje.

-module(session_manager_server).
-behaviour(gen_server).

%% API
-export([start/0,start_crawler/1, start_session/1, stop_session/0, stop_crawler/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {proplist}).

%% @type result() = term()
%% @type proplist() = [{Key :: string(), Value :: string()}]

%% @spec start() -> {ok,Pid :: pid()} | {error,term()}
%% @doc Uruchamia proces serwera.
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec start_crawler(PropList  :: proplist()) -> [ResultTuple]
%% ResultTuple = {crawl_event, result()} | {cache, result()} | {domain_manager, result()}
%% @doc Uruchamia wszystkie komponenty crawler'a, ktore beda braly udzial w sciaganiu tresci. PropList to lista parametrow konfiguracyjnych.
start_crawler(PropList) ->
    gen_server:call(?MODULE,{start_crawler,PropList}).

%% @spec start_session(PropList :: proplist()) -> [{scheduler, result()}]
%% @doc  Uruchamia sesje crawl'u. PropList to parametry konfiguracyjne dla tej sesji.
start_session(PropList) ->
    gen_server:call(?MODULE,{start_session,PropList}).

%% @spec stop_crawler() -> [ResultTuple]
%% ResultTuple = {crawl_event, result()} | {cache, result()} | {domain_manager, result()}
%% @doc Zatrzymuje komponenty crawler'a.
stop_crawler() ->
    gen_server:call(?MODULE,stop_crawler).

%% @spec stop_session() -> [{scheduler, result()}]
%% @doc Zatrzymuje sesje crawl'u.
stop_session() ->
    gen_server:call(?MODULE,stop_session).

%==============================CallBacks================================

%% @private
init([]) ->
    {ok, #state{proplist = undefined}}.

%% @private
handle_call({start_crawler,_PropList}, _From, State) ->
    {ok,PropList} = application:get_env(session_manager,prop_list),
    set_env_props(PropList),
    CrawlEventResult = start_crawl_event_app(),
    crawl_event:add_handler(session_event_handler,[get_remote_manager_server_node(PropList)]),
    log_crawl_event_rising(),
    DomainManagerResult = case get_domain_manager_node(PropList) =:= node() of
        true -> start_domain_manager_app(get_contact_nodes(PropList));
        _ -> void
    end,
    CacheAppResult = start_cache_app(),
    Reply = [{crawl_event,CrawlEventResult},{cache,CacheAppResult},{domain_manager,DomainManagerResult}],
    {reply, Reply, State#state{proplist = PropList}};

handle_call({start_session,PropList}, _From, State) ->
    set_env_props(PropList),
    SchedulerAppResult = start_scheduler_app(),
    %% case get_remote_manager_server_node(PropList) =:= node() of
	%%	true -> rpc:multicall(get_contact_nodes(PropList), session_manager, set_default_validity_time, [get_default_validity_time(PropList)]);
	%%	_ -> ok
	%%end,
    %session_manager:insert(get_init_url(PropList),get_depth(PropList),get_width(PropList),get_validity_time(PropList)),
    init_urls(get_init_urls( PropList)),
    {reply,[{scheduler,SchedulerAppResult}],State};

handle_call(stop_crawler,_From,State = #state{proplist = PropList}) ->
    CacheAppResult = application:stop(cache),
    DomainManagerResult = case get_domain_manager_node(PropList) =:= node() of
        true -> stop_domain_manager_app(get_contact_nodes(PropList));
        _ -> void
    end,
    CrawlEventResult = application:stop(crawl_event),
    {reply,[{crawl_event,CrawlEventResult},{cache,CacheAppResult},{domain_manager,DomainManagerResult}],State};

handle_call(stop_session,_From,State = #state{proplist = PropList}) ->
    SchedulerAppResult = application:stop(scheduler),
    application:stop(os_mon),
    application:stop(sasl),
    %% ContactNodes = get_contact_nodes(PropList),
    %% SchedulerAppResult = rpc:call(ContactNodes, application, stop, [scheduler]),
    %% rpc:call(ContactNodes, application, stop, [os_mon]),
    %% rpc:call(ContactNodes, application, stop, [sasl]),
    {reply,[{scheduler,SchedulerAppResult}],State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================Internal==================================

get_remote_manager_server_node(PropList) ->
    common:get_param(remote_manager_server_node,PropList).

get_width(PropList) ->
    common:get_param(width,PropList).

get_depth(PropList) ->
    common:get_param(depth,PropList).

get_validity_time(PropList) ->
    common:get_param(validity_time,PropList).

get_default_validity_time(PropList) ->
	common:get_param(default_validity_time, PropList).

get_contact_nodes(PropList) ->
    common:get_param(contact_nodes,PropList).

get_domain_manager_node(PropList) ->
    common:get_param(domain_manager_node,PropList).

get_init_urls(PropList) ->
    common:get_param(init_urls,PropList).

get_init_url(PropList) ->
    common:get_param(init_url,PropList).

get_max_process_count(PropList) ->
    common:get_param(max_process_count,PropList).

get_buffer_size(PropList) ->
    common:get_param(buffer_size,PropList).

get_trigger_time(PropList) ->
    common:get_param(trigger_time,PropList).
    
get_session_id(PropList) ->
	common:get_param(session_id, PropList).

init_urls([]) -> ok;
init_urls([PropList | T]) ->
	session_manager:insert(get_init_url(PropList),get_depth(PropList),get_width(PropList),get_validity_time(PropList)),
	init_urls(T).

set_env_props(PropList) ->
	application:set_env(cache, session_id, get_session_id(PropList)),
    application:set_env(session_manager,max_process_count,get_max_process_count(PropList)),
    application:set_env(session_manager,buffer_size,get_buffer_size(PropList)),
    application:set_env(session_manager,trigger_time,get_trigger_time(PropList)),
    application:set_env(session_manager,contact_nodes,get_contact_nodes(PropList)),
    application:set_env(session_manager,domain_manager_node,get_domain_manager_node(PropList)),
    application:set_env(session_manager,default_validity_time,get_default_validity_time(PropList)).

start_crawl_event_app() ->
    application:start(crawl_event).

start_domain_manager_app(Nodes) ->
    domain_manager_app:do_once(Nodes),
    rpc:multicall(Nodes,mnesia,start,[]),
    domain_manager_app:do_once1(Nodes),
    application:start(domain_manager).

start_cache_app() ->
    application:start(cache).

start_scheduler_app() ->
	application:start(sasl),
	application:start(os_mon),
  application:start(scheduler).
    %% rpc:multicall(Nodes, application, start, [sasl]),
    %% rpc:multicall(Nodes, application, start, [os_mon]),
    %% rpc:multicall(Nodes, application, start, [scheduler]).

log_crawl_event_rising() ->
    MsgContent = "Aplikacja logujac zdarzenia wstala\n",
    crawl_event:log_message({info,node(),crawl_event,start,MsgContent}).

stop_domain_manager_app(_Nodes) ->
    application:stop(domain_manager).
