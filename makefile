compile: compile_cache compile_test compile_riak-erlang-client compile_domain_manager compile_crawl_event compile_scheduler
	
clean: clean_cache clean_test clean_riak-erlang-client clean_domain_manager clean_crawl_event clean_scheduler
	
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
	
run:
	erl -pa ./crawl_test cache/ebin crawl_event/ebin riak-erlang-client/ebin riak-erlang-client/deps/*/ebin domain_manager/ebin scheduler/ebin -name michal@10.20.114.119 -mnesia dir '"cache/priv"' -mnesia dc_dump_limit 40 -mnesia dump_log_write_treshold 50000 -setcookie abc
	
			
