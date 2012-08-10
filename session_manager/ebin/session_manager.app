{application, cache,
 [{description, "Session manager for Crawler"},
  {vsn, "0.1.0"},
  {modules, [session_manager
             ]},
  {registered, [session_manager, session_manager_sup]},
  {applications, [kernel, stdlib]},
  {mod, {session_manager_app, []}}
 ]}.
