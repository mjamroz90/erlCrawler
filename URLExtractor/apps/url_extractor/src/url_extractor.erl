%% Author: maciek
-module(url_extractor).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([extract/2]).

%%
%% API Functions
%%
-export([extract_urls/3]).

extract(_,URL) ->
	ibrowse:start(),
	%{ok, _StatusCode, _Headers, WebPageText} = ibrowse:send_req(URL, [], get),
	%{match,List} = re:run(WebPageText,"(http\\://[:/?#\\[\\]@!%$&'()*+,;=a-zA-Z0-9._\\-~]+)",[global]),
	case ibrowse:send_req(URL, [], get) of
		{ok, _StatusCode, _Headers, WebPageText} ->
			case re:run(WebPageText,"(http\\://[:/?#\\[\\]@!%$&'()*+,;=a-zA-Z0-9._\\-~]+)",[global]) of
				{match, List} -> extract_urls(List,WebPageText,[]);
				nomatch -> []
			end;
		_ -> []
	end.
	%extract_urls(List,WebPageText,[]).

extract_urls([],_,Accum)-> 
	Accum;
extract_urls([[{Start,Len}|_]|T],Text,Accum)->
	extract_urls(T,Text,[lists:sublist(Text, Start+1, Len)|Accum]).


%%
%% Local Functions
%%

