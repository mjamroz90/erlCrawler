{application, session_manager,
 [{description, "Session manager for Crawler"},
  {vsn, "0.1.0"},
  {modules, [session_manager, session_manager_sup, session_manager_server, session_manager_app, session_manager
             ]},
  {registered, [session_manager, session_manager_sup, session_manager_server, session_manager_app]},
  {applications, [kernel, stdlib]},
  {mod, {session_manager_app, []}},
  {env,[{prop_list,[
   {max_process_count,20},
   {buffer_size,300},
   {trigger_time,200},
   {contact_nodes,['michal@127.0.0.1']},
   {domain_manager_node,'michal@127.0.0.1'},
   {remote_manager_server_node,'michal@127.0.0.1'},
   {init_url,"www.google.pl"},
   {width,2},
   {depth,3},
   {validity_time,2000}
  ]}]}
 ]}.
