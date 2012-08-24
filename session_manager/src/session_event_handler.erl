-module(session_event_handler).
-behaviour(gen_event).
-export([init/1,handle_event/2,code_change/3,terminate/2,handle_info/2,handle_call/2]).


%% @type proplist() = [{Key :: term(), Value :: term()}]
%% @type state() = node()
%% @type info_type() = info | warning | error

%% @spec init([ HttpServerNode :: node()]) -> {ok,HttpServerNode :: node()}
%% @doc Jest wywolywana w czasie rejestracji handlera, argumentem jest wezel, na ktorym znajduje sie serwer protokolu Http.
init([HttpServerNode]) ->
    {ok,HttpServerNode}.

%% 	@spec handle_event({log_message,{Msg_Type :: info_type(),Node :: node(),AppName :: atom(),EventName :: atom(),Content :: string()}},NodeToReport :: state()) -> {ok,HttpServerNode :: state()}
%%  @doc Przechwytuje zdarzenie logowania przez aplikacje i wysyla log do procesu, ktory przesle go dalej, do aplikacji webowej.
handle_event({log_message,{Msg_Type,Node,AppName,EventName,Content}},HttpServerNode) ->
    case HttpServerNode =:= node() of
        true ->  reporting_server:log_message({Msg_Type,Node,AppName,EventName,Content});
        false -> gen_server:cast({reporting_server,HttpServerNode},{log_message,{Msg_Type,Node,AppName,EventName,Content}})
    end,
    {ok,HttpServerNode};

handle_event(_Msg,HttpServerNode) ->
    {ok,HttpServerNode}.

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


