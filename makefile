run:
	erl -pa cache/ebin riak-erlang-client/ebin riak-erlang-client/deps/*/ebin domain_manager/ebin 
			-name 'michal@192.168.1.103' -mnesia dir cache/priv