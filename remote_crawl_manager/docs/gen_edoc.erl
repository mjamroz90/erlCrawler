-module(gen_edoc).
-export([generate/0]).
-define(OUTPUT_DIR,"./docs").

generate() ->
	case file:list_dir("./src/impl") of
		{ok,FileNames} -> 			
			edoc:files(lists:map(fun(File) -> filename:join("src/impl",File) end, FileNames),[{dir,?OUTPUT_DIR}]);
		Error -> Error
	end.	
