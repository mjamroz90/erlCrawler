{application, crawl_event,
 [{description, "Event manager for Crawler"},
  {vsn, "0.1.0"},
  {modules, [crawl_event,
			 crawl_event_sup,
			 crawl_event_app
             ]},
  {registered, [crawl_event,crawl_event_sup,crawl_event_app]},
  {applications, [kernel, stdlib]},
  {mod, {crawl_event_app, []}}
 ]}.
