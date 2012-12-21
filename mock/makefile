compile:
	erlc -o ./ebin ./src/*.erl
	
clean: 
	rm -rf ./ebin/*.beam

clean_doc:
	rm -rf ./docs/*.html ./docs/*.css

edoc:
	erl -pa ./docs -run gen_edoc generate -run init stop -noshell


