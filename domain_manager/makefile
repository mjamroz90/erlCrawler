compile:
	erlc -o ./ebin ./src/*.erl
	#erlc -o ./ebin ./test/*.erl
run:
	erl -pa ./ebin -mnesia dir '"./priv"' -name michal@192.168.1.103 -setcookie abc
	
clean: 
	rm ./ebin/*.beam



