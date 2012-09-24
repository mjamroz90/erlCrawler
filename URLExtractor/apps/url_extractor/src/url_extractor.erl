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
-export([extract_urls/3,forward/1,forward_headers/2,get_links/1]).

extract(ID,URL) ->
	case ibrowse:send_req(URL, [], get) of 
		{ok, "200", _Headers, Body} -> get_links(Body);
        {ok, "301", Headers, _B} -> forward_headers(ID, Headers);
        _ -> []
	end.

get_links(Body) ->
	case re:run(Body,"(http\\://[:/?#\\[\\]@!%$&'()*+,;=a-zA-Z0-9._\\-~]+)",[global]) of
		{match,List} -> extract_urls(List,Body,[]);
		nomatch -> []
	end.
	

extract_urls([],_,Accum)-> 
	Accum;
extract_urls([[{Start,Len}|_]|T],Text,Accum)->
	extract_urls(T,Text,[lists:sublist(Text, Start+1, Len)|Accum]).

forward_headers(ID,Headers) ->
	case forward(Headers) of
		nope -> bad_redirect;
		URL -> extract(ID,URL)
	end.

forward([])->
	nope;
forward([{"location",URL} | _T])->
	URL;
forward([{"Location",URL} | _T])->
	URL;
forward([_H|T])->
	forward(T).
		

%%
%% Local Functions
%%

