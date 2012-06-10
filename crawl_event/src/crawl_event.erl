-module(crawl_event).
-export([start/0,add_handler/2,delete_handler/2,report_load/1]).

start() ->
	gen_event:start_link({local,?MODULE}).
	
add_handler(Handler,Args) ->
	gen_event:add_handler(?MODULE,Handler,Args).
	
delete_handler(Handler,Args) ->
	gen_event:delete_handler(?MODULE,Handler,Args).
	
report_load(Load) ->
	gen_event:notify(?MODULE,{report_load,{node(),Load}}).
	
