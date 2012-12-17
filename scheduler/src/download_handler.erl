%to jest mock modul, ktory ma byc zastapiony przez system erlCrawler, aby zasymulowac jego dzialanie, nalezy wywolac funkcje start

-module(download_handler).

-export([start/0, download/0]).

start() ->
	spawn(download_handler, download, []).

download() ->
	Urls = url_download_server:pull(1000),
	report(Urls),
	
	timer:sleep(500),
	download().
	
report([{UrlId, Url} | T]) ->
	url_download_server:report(UrlId, Url, term_to_binary(empty), ok),
	report(T);
report([]) ->
	ok.
	
	
