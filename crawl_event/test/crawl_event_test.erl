-module(crawl_event_test).
-include_lib("eunit/include/eunit.hrl").
-define(TEST_MODULE_NAME,crawl_event).
-define(MOCK_EVENT_HANDLER,mock_event_handler).

crawl_event_test() ->
	{setup,
	 fun setup/0,
	 fun cleanup/1,
	 [
		fun test_add_delete_handler/0,
		fun test_event_notifications/0
	 ]
	}.
	
setup() ->
	?TEST_MODULE_NAME:start().
	
test_add_delete_handler() ->
	?assertMatch([],gen_event:which_handlers(?TEST_MODULE_NAME)),
	crawl_event:add_handler(?MOCK_EVENT_HANDLER,[self()]),
	?assertMatch([?MOCK_EVENT_HANDLER],gen_event:which_handlers(?TEST_MODULE_NAME)),
	crawl_event:delete_handler(?MOCK_EVENT_HANDLER,[]),
	?assertMatch([],gen_event:which_handlers(?TEST_MODULE_NAME)).
	
test_event_notifications() ->
	crawl_event:add_handler(?MOCK_EVENT_HANDLER,[self()]),
	crawl_event:report_load(1234),
	Msg = receive
		Msg_1 -> Msg_1
	end,
	?assertMatch(Msg,{report_load,1234}),
	Message = "Message",
	crawl_event:log_message(Message),
	Msg1 = receive
		Msg_2 -> Msg_2
	end,
	?assertMatch(Msg1,{log_message,Message}),
	crawl_event:report_stats(Message,true),
	Msg2 = receive
		Msg_3 -> Msg_3
	end,
	?assertMatch(Msg2,{report_stats,Message}).
	
	
cleanup(_) ->
	gen_event:stop(?TEST_MODULE_NAME).
