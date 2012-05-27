{application, domain_manager,
 [{description, "Domain Manager for erlCrawler"},
  {vsn, "0.1.0"},
  {modules, [domain_manager_app,
			 domain_manager_sup,
			 domain_dispatch_server
             ]},
  {registered, [domain_manager_sup, domain_dispatch_server]},
  {applications, [kernel, stdlib]},
  {mod, {domain_manager_app, []}}
 ]}.
