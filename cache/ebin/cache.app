{application, cache,
 [{description, "Caching system for Crawler"},
  {vsn, "0.1.0"},
  {modules, [ram_cache_server,
			 gen_ram_cache_server,
			 domain_ram_cache_server,
			 domain_cache_server,
             disk_cache_server,
             riak_disk_cache_server,
             eleveldb_disk_cache_server,
			 url_server,
             cache_sup,
			 domain_cache_sup,	
             cache_app,
             main_sup,
             eleveldb_main_sup,
             eleveldb_worker_sup,
             eleveldb_disk_sup,
             eleveldb_worker,
             visited_urls_server
             ]},
  {registered, [ram_cache_server,domain_ram_cache_server,
				eleveldb_disk_cache_server,domain_cache_server,cache_sup,
				domain_cache_sup,main_sup,visited_urls_server,eleveldb_worker_sup,eleveldb_main_sup,eleveldb_disk_sup]},
  {applications, [kernel, stdlib, crawl_event]},
  {mod, {cache_app, []}},
  {env, [{eleveldb_env,[{open_opts,[{create_if_missing, true}, 
					{error_if_exists, false},
					{write_buffer_size, 4194304},
					{max_open_files, 20},
					{block_size, 32768},
					{cache_size, 16777216}]},
					
					{root_dir,"./db"},
					{partition_num,4}]
					},
										
			{riak_env,[{node_name,"127.0.0.1"},
					  {port, 8087},
					  {max_item_num_cache,1000000},
					  {max_item_num_domain,100000}
					  ]},
			{session_id, 0}
					  
		]}
 ]}.
