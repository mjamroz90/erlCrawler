compile: compile_cache compile_test compile_riak-erlang-client compile_domain_manager compile_crawl_event compile_scheduler compile_eleveldb
	
clean: clean_cache clean_test clean_riak-erlang-client clean_domain_manager clean_crawl_event clean_scheduler clean_eleveldb
	
compile_cache:
	cd ./cache; make compile
	
compile_riak-erlang-client:
	cd ./riak-erlang-client; make
	
compile_domain_manager:
	cd ./domain_manager; make compile

compile_crawl_event:
	cd ./crawl_event; make compile
	
compile_scheduler:
	cd ./scheduler; make compile

compile_test:
	cd ./crawl_test; make compile
	
compile_eleveldb:
	cd ./eleveldb; make compile	
		
clean_cache:
	cd ./cache; make clean
	
clean_riak-erlang-client:
	cd ./riak-erlang-client; make clean
	
clean_domain_manager:
	cd ./domain_manager; make clean

clean_crawl_event:
	cd ./crawl_event; make clean	
	
clean_scheduler:
	cd ./scheduler; make clean
	
clean_test:
	cd ./crawl_test; make clean
	
clean_eleveldb:
	cd ./eleveldb; make clean
	
run:
	erl -pa ./crawl_test cache/ebin crawl_event/ebin riak-erlang-client/ebin riak-erlang-client/deps/*/ebin domain_manager/ebin scheduler/ebin eleveldb/ebin -name michal@192.168.1.101 -mnesia dir '"cache/priv"' -mnesia dc_dump_limit 40 -mnesia dump_log_write_treshold 50000 -setcookie abc
	
			
