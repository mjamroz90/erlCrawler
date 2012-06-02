-module(speedtest).
-export([test/0,test_db/2, test_url/2, start/3]).

test() ->
	application:start(cache),
	Time = test_db("urls", 30),
	io:format("~p~n", [Time]).

test_db(FileName, Copies) ->
	test(disk_cache_server, FileName, Copies).
	
test_url(FileName, Copies) ->
	test(url_server, FileName, Copies).

test(Object, FileName, Copies) ->
	case file:open(FileName, read) of
		{ok, File} ->
			{Time, _} = timer:tc(?MODULE, start, [Object, File, Copies]),
			disk_cache_server:delete_all(),
			%ram_cache_server:stop(),
			Time;
			%start(File, Copies);
		{error, Reason} -> {error, Reason}
	end.
	
start(Object, File, Copies) ->
	case file:read_line(File) of
		{ok, Line} ->
			%io:format("~p", [string:sub_string(Line,0, string:len(Line)-2) ++ "AABB"]),
			insert(Object, string:substr(Line,1, string:len(Line)-1), Copies),
			start(Object, File, Copies);
		eof -> ok;
		{error, Reason} -> {error, Reason}
	end.
	
	
insert(_Object, _Url, 0) -> ok;
insert(Object, Url, N) ->
	%io:format("~p\n", [Url ++ integer_to_list(N)]),
	Object:insert(Url ++ integer_to_list(N), {empty}),
	insert(Object, Url, N-1).
	
