-module(ram_cache_test).
-include_lib("eunit/include/eunit.hrl").

ram_cache_test_() ->
	[fun delete_insert_lookup/0].

setup() -> ram_cache:new(?MODULE).

cleanup(Tab) -> ram_cache:remove(Tab).

delete_insert_lookup() ->
	Tab = setup(),
	Key = "abc",
	Key1 = "def",	
	Value = [1,2,3],
	Value1 = [4,5,6],
	ram_cache:insert(Tab,Key,Value),
	ram_cache:insert(Tab,Key1,Value1),
	?assertEqual(true,ram_cache:exists(Tab,Key)),
	?assertEqual(ram_cache:lookup(Tab,Key),Value),
	?assertEqual(ram_cache:tab_size(Tab),2),
	ram_cache:delete(Tab,Key),
	?assertEqual(false,ram_cache:exists(Tab,Key)),	
	ram_cache:remove_random(Tab),
	?assertEqual(0,ram_cache:tab_size(Tab)),
	cleanup(Tab).
