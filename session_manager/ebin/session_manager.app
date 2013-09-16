{application, session_manager,
  [{description, "Session manager for Crawler"},
    {vsn, "0.1.0"},
    {modules, [session_manager, session_manager_sup, session_manager_server, session_manager_app, session_manager
    ]},
    {registered, [session_manager, session_manager_sup, session_manager_server, session_manager_app]},
    {applications, [kernel, stdlib]},
    {mod, {session_manager_app, []}},
    {env, [{prop_list, [
      {session_id, 0},
      {max_process_count, 7},
      {buffer_size, 3000},
      %%{trigger_time,20000},
      {contact_nodes, ['node1@127.0.0.1']},
      {domain_manager_node, 'node1@127.0.0.1'},
      {remote_manager_server_node, 'node1@127.0.0.1'},
      %%{init_url,"www.google.pl"},
      %%{width,2},
      %%{depth,3},
      %%{validity_time,2000},
      {init_urls, [
        [
          {init_url, "http://www.allegro.pl"},
          {width, 1},
          {depth, 999999999},
          {validity_time, 86400000000},

          {subdomain_breadth, 1},
          {subdomain_depth, 999999999},
          {subdomain_validity_time, 86400000000}

        ]
      ]
      },
      {default_validity_time, 86400000000}, %24hours
      {default_breadth, 1},
      {default_depth, 1}

    ]}]}
  ]}.
