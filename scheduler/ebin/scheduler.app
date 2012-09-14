{application, scheduler,
 [{description, "Scheduler for Crawler"},
  {vsn, "0.1.0"},
  {modules, [scheduler,
			 scheduler_sup,
			 scheduler_app,
			 processing_sup,
			 url_processing,
			 reg,
			 reg_sup,
			 trigger,
			 trigger_sup
             ]},
  {registered, [scheduler, scheduler_app, scheduler_sup, processing_sup, reg, reg_sup, trigger, trigger_sup]},
  {applications, [kernel, stdlib, cache, sasl, os_mon]},
  {mod, {scheduler_app, []}},
  {env,[
        {max_process_count, 5},
        {buffer_size, 1000},
        {trigger_time, 3000}
  ]}
 ]}.
