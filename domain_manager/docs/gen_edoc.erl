-module(gen_edoc).
-export([generate/0]).
-define(OUTPUT_DIR,"./docs").

generate() ->
	case file:list_dir("./src") of
		{ok,FileNames} -> 			
			edoc:files(lists:map(fun(File) -> filename:join("src",File) end, FileNames),[{dir,?OUTPUT_DIR}]);		
		Error -> Error
	end.	
