%% @doc Modul zawierajacy wspolne funkcje aplikacji wchodzacych w sklad systemu.
%% @end
-module(common).
-export([timestamp/0, get_param/2, stick_params/2]).

%% @type proplist() = [{Key::term(), Value::term()}] 

%% @spec timestamp() -> integer()
%% @doc Zwraca aktualny znacznik czasu (unikalny).
%% @end
timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.

%% @spec get_param(ParamName :: term(), ParamList :: proplist()) -> term() | not_found
%% @doc Zwraca wartosc wybranego parametru z listy parametrow.
%% @end
get_param(ParamName,ParamList) ->
	case lists:keyfind(ParamName,1,ParamList) of
		false -> not_found;
		{_,Value} -> Value
	end.

%% @spec stick_params(NewParam :: {ParamName :: term(), Value :: term()}, OldParams :: proplist()) -> proplist()
%% @doc Dolacza do listy lub zamienia w liscie okreslony parametr i wartosc.
%% @end
stick_params(NewTuple,Params) ->	
	NewParams = lists:keydelete(element(1,NewTuple),1,Params),
	[NewTuple | NewParams].
