{application, cache,
 [{description, "Caching system for Crawler"},
  {vsn, "0.1.0"},
  {modules, [ram_cache_server,
             disk_cache_server,			 
             cache_sup,
             cache_app		 
             ]},
  {registered, [ram_cache_server,disk_cache_server,cache_sup]},
  {applications, [kernel, stdlib]},
  {mod, {cache_app, []}}
 ]}.
