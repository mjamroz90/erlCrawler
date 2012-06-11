compile: compile_cache compile_riak-erlang-client compile_domain_manager compile_crawl_event compile_scheduler
	
clean: clean_cache clean_riak-erlang-client clean_domain_manager clean_crawl_event clean_scheduler
	
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
	
run:
	erl -pa cache/ebin crawl_event/ebin riak-erlang-client/ebin riak-erlang-client/deps/*/ebin domain_manager/ebin scheduler/ebin -name michal@192.168.1.103 -mnesia dir '"cache/priv"' -setcookie abc
			
