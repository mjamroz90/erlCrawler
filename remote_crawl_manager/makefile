run:
	erl -pa ./ebin -name michal@192.168.1.102

compile:  compile_idl

	erlc +debug_info -o ./ebin ./src/generated/*.erl ./src/impl/*.erl

compile_idl:
	erlc -o ./src/generated ./src/idl/*.idl

clean: 
	rm ./ebin/*.beam

clean_doc:
	rm ./docs/*.html ./docs/*.css

edoc:
	erl -pa ./docs -run gen_edoc generate -run init stop -noshell



