compile:
	erlc -o ./ebin ./src/*.erl
	erlc -o ./ebin ./test/*.erl
run:
	erl -pa ./ebin ../riak-erlang-client/deps/*/ebin/ ../riak-erlang-client/ebin/
	
clean: 
	rm ./ebin/*.beam



