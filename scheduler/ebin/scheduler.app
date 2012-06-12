{application, scheduler,
 [{description, "Scheduler for Crawler"},
  {vsn, "0.1.0"},
  {modules, [scheduler,
			 scheduler_sup,
			 scheduler_app,
			 processing_sup,
			 url_processing,
			 reg,
			 reg_sup
             ]},
  {registered, [scheduler, scheduler_app, scheduler_sup, processing_sup, reg, reg_sup]},
  {applications, [kernel, stdlib, cache]}, %TODO -add domain_manager
  {mod, {scheduler_app, []}}
 ]}.
