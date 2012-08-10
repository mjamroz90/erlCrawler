-module(common).
-export([timestamp/0, get_param/2, stick_params/2]).


timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.

get_param(ParamName,ParamList) ->
	case lists:keyfind(ParamName,1,ParamList) of
		false -> not_found;
		{_,Value} -> Value
	end.

stick_params(NewTuple,Params) ->	
	NewParams = lists:keydelete(element(1,NewTuple),1,Params),
	[NewTuple | NewParams].
