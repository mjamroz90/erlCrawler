{application, cache,
 [{description, "Caching system for Crawler"},
  {vsn, "0.1.0"},
  {modules, [ram_cache_server,
			 gen_ram_cache_server,
			 domain_ram_cache_server,
			 domain_cache_server,
             disk_cache_server,
			 url_server,
             cache_sup,
			 domain_cache_sup,	
             cache_app,
             main_sup			 
             ]},
  {registered, [ram_cache_server,domain_ram_cache_server,
				disk_cache_server,domain_cache_server,cache_sup,domain_cache_sup,main_sup]},
  {applications, [kernel, stdlib]},
  {mod, {cache_app, []}}
 ]}.
