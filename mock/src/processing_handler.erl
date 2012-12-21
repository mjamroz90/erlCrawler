%to jest mock modul, ktory ma byc zastapiony przez system erlCrawler

-module(processing_handler).
-export([process_data/2, process_data/3]).

process_data(Id, Url) ->
	mockparser:mockparse3(Id, Url, 100).
	
process_data(Id, Url, _Source) ->
	process_data(Id, Url).
