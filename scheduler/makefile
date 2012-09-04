compile:
	erlc -o ./ebin ./src/*.erl
	
clean: 
	rm ./ebin/*.beam

clean_doc:
	rm ./docs/*.html ./docs/*.css

edoc:
	erl -pa ./docs -run gen_edoc generate -run init stop -noshell


