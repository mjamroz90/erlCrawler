%% @doc Serwer, ktory zbiera logi z crawler'a i wysyla je do aplikacji webowej. Na razie nie zaimplementowany.

-module(reporting_server).
-behaviour(gen_server).

%% API
-export([start/1,log_message/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {web_app_controller_url}).

%%=========================================== API ======================================

%% @spec start(WebAppCtrlUrl :: string()) -> {ok,Pid :: pid()} | {error, term()}
%% @doc Startuje serwer z argumentem okreslajacym nazwe kontroler'a obslugujacego zdarzenia przychodzace.
start(WebAppCtrlUrl) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [WebAppCtrlUrl], []).

%% @spec log_message(Msg :: tuple()) -> void
%% @doc Funkcja ta przesyla log do serwera.
log_message(Msg) ->
    gen_server:cast(?SERVER,{log_message,Msg}).

%% @spec stop() -> ok
%% @doc Zatrzymuje serwer.
stop() ->
    gen_server:cast(?SERVER,stop).

%%======================================== CallBacks ===================================

%% @private
init([WebAppCtrlUrl]) ->
    {ok, #state{web_app_controller_url = WebAppCtrlUrl}}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({log_message,Msg}, State = #state{web_app_controller_url = WebAppCtrlUrl}) ->
    io:format("Otrzymalem wiadomosc - ~p\n",[Msg]),
    {noreply, State};

handle_cast(stop,State) ->
    {stop,"Made to stop",State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.