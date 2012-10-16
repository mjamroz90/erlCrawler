-module(stats).
-export([start/2, report/0, get_total_counter/0, get_part_counter/0, get_mean_speed/0, get_part_mean_speed/0, stop/0]).
-record(state,{file_descr, start_time, total_counter, part_size, part_start_time, part_counter, part_mean_speed}).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
    
-define(URL_BUCKET,term_to_binary("Urls")).
    
%======================================API==================================
         
start(LogToFile, PartSize) ->
	gen_server:start_link({local,?MODULE},?MODULE,[LogToFile,PartSize],[]).
	
report() ->
	gen_server:cast(?MODULE, report).
	
get_total_counter() ->
	gen_server:call(?MODULE, get_total_counter).
	
get_part_counter() ->
	gen_server:call(?MODULE, get_part_counter).
	
get_mean_speed() ->
	gen_server:call(?MODULE, get_total_mean_speed).
	
get_part_mean_speed() ->
	gen_server:call(?MODULE, get_part_mean_speed).

stop() ->
	gen_server:cast(?MODULE,stop).
	
%===================================Callbacks==============================

init([LogToFile, PartSize]) ->
	Descr = case LogToFile of
		true ->
			case file:open("stats", [write]) of
				{ok, IoDevice} ->
					IoDevice;
				{error, _Reason} ->
					false
			end;
		false -> false
	end,
	{ok,#state{file_descr = Descr, start_time = 0, total_counter = 0,
		part_size = PartSize, part_start_time = 0, part_counter = 0, part_mean_speed = 0}}.
		
handle_cast(stop,State) ->
	{stop,"Made to stop",State};

handle_cast(report, State = #state{file_descr = Descr, start_time = StartTime, total_counter = TotalCounter,
	part_size = PartSize, part_start_time = PartStartTime, part_counter = PartCounter, part_mean_speed = PartMeanSpeed}) ->
	
	case TotalCounter of
		0 ->
			NewStartTime = common:timestamp();
		_ ->
			NewStartTime = StartTime
	end,
	
	case PartCounter of
		0 ->
			NewPartStartTime = common:timestamp();
		_ ->
			NewPartStartTime = PartStartTime
	end,
		
	case PartCounter + 1 of
		PartSize ->
			NewPartMeanSpeed = (common:timestamp() - PartStartTime)/PartSize,
			TotalMeanSpeed = (common:timestamp() - StartTime)/(TotalCounter+1),
			log_to_file(Descr, TotalCounter+1, TotalMeanSpeed, NewPartMeanSpeed);
		_ -> 
			NewPartMeanSpeed = PartMeanSpeed
	end,
	
	{noreply, State#state{start_time = NewStartTime, total_counter = TotalCounter + 1, part_start_time = NewPartStartTime,
		part_counter = (PartCounter + 1) rem PartSize, part_mean_speed = NewPartMeanSpeed}}.
			
	
handle_call(get_total_counter, _From, State = #state{total_counter = TotalCounter}) ->
	{reply,TotalCounter,State};
	
handle_call(get_part_counter, _From, State = #state{part_counter = PartCounter}) ->
	{reply, PartCounter, State};
	
handle_call(get_total_mean_speed, _From, State = #state{total_counter = TotalCounter, start_time = StartTime}) ->
	MeanSpeed = case TotalCounter of
		0 ->
			0;
		N ->
			(common:timestamp()-StartTime)/TotalCounter
	end,
	{reply, MeanSpeed, State};
	
handle_call(get_part_mean_speed, _From, State = #state{part_mean_speed = PartMeanSpeed}) ->
	{reply, PartMeanSpeed, State}.
			
handle_info(_Msg,State) ->
	{noreply,State}.
	
terminate(_Reason,#state{file_descr = Descr}) ->
	case Descr of
		false ->
			ok;
		IoDevice ->
			file:close(IoDevice)
	end.	
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
	

log_to_file(Descr, TotalCounter, TotalMeanSpeed, PartMeanSpeed) ->
	IoDevice = case Descr of
		false ->
			standard_io;
		Device ->
			Device
	end,
	io:format(IoDevice,"~p ~p ~p ~p ~p~n", [TotalCounter, PartMeanSpeed, 1000000/PartMeanSpeed, TotalMeanSpeed, 1000000/TotalMeanSpeed]).
