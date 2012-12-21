compile: compile_cache compile_test compile_riak-erlang-client compile_domain_manager compile_crawl_event compile_scheduler compile_eleveldb compile_session_manager compile_remote_manager

compile_mocks: compile compile_mock
	
clean: clean_cache clean_test clean_riak-erlang-client clean_domain_manager clean_crawl_event clean_scheduler clean_eleveldb clean_session_manager clean_remote_manager clean_mock

clean_db:
	rm -rf ./db/*
	rm -rf ./cache/priv/*
	
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

compile_session_manager:
	cd ./session_manager; make compile

compile_remote_manager:
	cd ./remote_crawl_manager; make compile

compile_mock:
	cd ./mock; make compile
	
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
	
clean_session_manager:
	cd ./session_manager; make clean

clean_remote_manager:
	cd ./remote_crawl_manager; make clean
	
clean_mock:
	cd ./mock; make clean
	
run:
	erl -pa ./crawl_test cache/ebin crawl_event/ebin riak-erlang-client/ebin riak-erlang-client/deps/*/ebin domain_manager/ebin scheduler/ebin eleveldb/ebin session_manager/ebin remote_crawl_manager/ebin ./ErlCrawler/apps/crawler_app/ebin ./ErlCrawler/deps/ibrowse/ebin/ ./ErlCrawler/deps/lager/ebin/ ./ErlCrawler/deps/mochiweb/ebin/ -name node1@127.0.0.1 -mnesia dir '"cache/priv"' -mnesia dc_dump_limit 40 -mnesia dump_log_write_treshold 50000 -setcookie abc
	
run_mocks:
	erl -pa ./crawl_test cache/ebin crawl_event/ebin riak-erlang-client/ebin riak-erlang-client/deps/*/ebin domain_manager/ebin scheduler/ebin eleveldb/ebin session_manager/ebin remote_crawl_manager/ebin mock/ebin -name node1@127.0.0.1 -mnesia dir '"cache/priv"' -mnesia dc_dump_limit 40 -mnesia dump_log_write_treshold 50000 -setcookie abc
			
