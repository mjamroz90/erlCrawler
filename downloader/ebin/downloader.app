{application, downloader,
 [{description, "Downloading system for Crawler"},
  {vsn, "0.1.0"},
  {modules, [download_worker,
			 downloader_app,
			 pid_storage_server,
			 url_downloader,
             url_downloader_sup
             ]},
  {registered, [url_downloader_sup,pid_storage_server,url_downloader]},				
  {applications, [kernel, stdlib, inets]},
  {mod, {downloader_app, []}},
  {env, [{conn_timeout,20000},
  		 {idle_timeout,5000},
  		 {max_workers_num,4}
		]
	}	
 ]}.
