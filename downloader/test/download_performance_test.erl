-module(download_performance_test).
-compile(export_all).



start(FileName,Num,ThreadNum) ->
	inets:start(),
	application:start(downloader),
	Urls = load_urls(FileName,Num),
	ChunkSize = round(Num/ThreadNum),
	Self = self(),
	{_,Pids} = lists:foldl(fun(_Num,{Indx,Pids}) ->
		UrlsChunk = lists:sublist(Urls,Indx,ChunkSize),
		Pid = spawn(fun() -> download_content(UrlsChunk,Self) end),
		{Indx+ChunkSize,[Pid|Pids]}

		end
		,{1,[]},lists:seq(1,ThreadNum)),
	lists:foreach(fun(Pid) -> Pid ! start end, Pids),
	{Time,_} = timer:tc(fun() -> wait_for_finish(length(Pids)) end),
	Time/1000.

wait_for_finish(PidsNum) ->
	wait_for_finish(PidsNum,0).

wait_for_finish(PidsNum,PidsNum) ->
	void;

wait_for_finish(PidsNum,Collected) ->
	receive
		{finish,_Pid} ->
			wait_for_finish(PidsNum,Collected+1)				
	end.
		
load_urls(FileName,Num) ->
	{ok,Descr} = file:open(FileName,[read]),
	Urls = get_list(Descr,Num),
	file:close(Descr),
	Urls.


get_list(_,0) ->	
	[];
get_list(Descr,Num) ->
	Url = io:get_line(Descr,""),
	[string:sub_string(Url,1,string:len(Url)-1) | get_list(Descr,Num-1)].	



download_content(Urls,Pid) ->
	receive 
		start -> void	
	end,	
	lists:foreach(fun(Url) -> {Res,_} = url_downloader:download_content(Url), io:format("~s\n",[Res]) end, Urls),
	Pid ! {finish,self()}.


check_for_access(FileName,HowMany,Output) ->
	inets:start(),
	application:start(downloader),
	{ok,Descr} = file:open(FileName,[read]),
	{ok,OutDescr} = file:open(Output,[write]),
	check(Descr,OutDescr,HowMany).	

check(InDescr,OutDescr,0) ->
	file:close(InDescr),
	file:close(OutDescr);

check(InDescr,OutDescr,HowMany) ->
	Url = io:get_line(InDescr,""),
	Url1 = string:sub_string(Url,1,string:len(Url)-1),
	Counter = case url_downloader:download_content(Url1) of 
		{download_error, _} -> HowMany;
		{Url1, _} -> file:write(OutDescr,Url1), io:nl(OutDescr), HowMany-1
	end,
	check(InDescr,OutDescr,Counter).	
