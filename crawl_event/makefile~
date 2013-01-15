compile:
	erlc -o ./ebin ./src/*.erl
	# erlc -o ./ebin ./test/*.erl

clean: 
	rm ./ebin/*.beam

run:
	erl -pa ./ebin ../domain_manager/ebin -name michal@192.168.1.101 -setcookie abc

clean_doc:
	rm ./docs/*.html ./docs/*.css

edoc:
	erl -pa ./docs -run gen_edoc generate -run init stop -noshell


