-module(processing_handler).
-export([process_data/2]).

process_data(_UrlId, Url) ->
	case url_downloader:download_content(Url) of
		{download_error,Url} -> [];
		{_Url,{_PlainText,UrlList}} -> UrlList
	end.
	
