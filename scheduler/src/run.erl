-module(run).
-export([main/0, work/0]).

main() ->
	application:start(erlCrawler),
	application:start(cache),
	application:start(crawl_event),
	application:start(domain_manager),
	application:start(scheduler).
	
work() ->
	application:start(erlCrawler),
	application:start(cache),
	application:start(crawl_event),
	application:start(scheduler).
