compile:
	erlc -o ./ebin ./src/*.erl
	#erlc -o ./ebin ./test/*.erl
run:
	erl -pa ./ebin -mnesia dir '"./priv"' -name michal1@192.168.1.101 -setcookie abc
	
clean: 
	rm ./ebin/*.beam



