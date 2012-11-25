%% @doc Serwer, ktory zbiera logi z crawler'a i wysyla je do aplikacji webowej. Na razie nie zaimplementowany.

-module(reporting_server).
-behaviour(gen_server).

%% API
-export([start/1,log_message/1, stop/0, report_stats/1]).

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

report_stats(Msg) ->
    gen_server:cast(?SERVER,{report_stats,Msg}).

%%======================================== CallBacks ===================================

%% @private
init([WebAppCtrlUrl]) ->
    inets:start(),
    {ok, #state{web_app_controller_url = WebAppCtrlUrl}}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({log_message,Msg}, State = #state{web_app_controller_url = WebAppCtrlUrl}) ->
    io:format("Otrzymalem wiadomosc - ~p\n",[Msg]),
    {noreply, State};

handle_cast({report_stats,Msg}, State = #state{web_app_controller_url = WebAppCtrlUrl}) ->
    {Str,Sum} = serialize_to_string(Msg),
    NodeParam = lists:flatten(io_lib:format("~p=~p",[hashValue,compute_hash(Sum)])),
    Body = string:concat(Str,NodeParam),
    DataType = "application/x-www-form-urlencoded",
    httpc:request(post,{WebAppCtrlUrl,[],DataType,Body},[],[]),
    io:format("Body=\n~p\nSum=~p",[Body,Sum]),
    {noreply,State};

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


%% @private
serialize_to_string(Msg) ->
  {Str,Sum} = lists:foldl(fun({Key,Value},{Acc,Val}) ->
                Param = lists:flatten(io_lib:format("~p=~p&",[Key,Value])),
                Val1 = case Key =:= nodeName of
                      true -> Val;
                      false -> Val+Value
                end,
                {string:concat(Acc,Param),Val1} end,{"",0},Msg),
  {Str,Sum}.

%% @private
compute_hash(Sum) ->
  Bin = binary:encode_unsigned(round(Sum)),
  Pass = "ecm",
  Hash = [X bxor Y || X <- binary_to_list(Bin),Y <- Pass],
  binary:decode_unsigned(list_to_binary(Hash)).
