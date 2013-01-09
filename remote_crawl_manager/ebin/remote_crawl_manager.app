{application, remote_crawl_manager,
 [{description, "Remote crawl manager for Crawler"},
  {vsn, "0.1.0"},
  {modules, [reporting_server,remote_crawl_manager_app,remote_crawl_manager_sup, corba_object_sup
             ]},
  {registered, [reporting_server,remote_crawl_manager_sup,corba_object_sup]},
  {applications, [kernel, stdlib]},
  {mod, {remote_crawl_manager_app, []}},
  {env,[{web_app_controller_url,"http://localhost:8080/ecm/stats"}]}
 ]}.
