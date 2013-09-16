-module(session_manager_test).
-include_lib("eunit/include/eunit.hrl").

session_manager_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      ?_test(subdomain_params()),
      ?_test(validity_time())
    ]
  }.

setup() ->
  reg:start_link(),
  application:start(session_manager).

cleanup(_) ->
  application:stop(session_manager).


subdomain_params() ->
  Domain1 = "test.test",
  Params1 = {2, 1},
  session_manager:set_subdomain_params(Domain1, element(1, Params1), element(2, Params1)),

  Domain2 = "aaa.bbb",
  Params2 = {3, 4},
  session_manager:set_subdomain_params(Domain2, element(1, Params2), element(2, Params2)),

  ?assertEqual(Params2, session_manager:get_subdomain_params(Domain2)),
  ?assertEqual(Params1, session_manager:get_subdomain_params(Domain1)).

validity_time() ->
  Url = "c.d",
  Domain = "c.d",
  Subdomain = "a.b.c.d",
  OtherDomain = "z.x.c.v",

  session_manager:set_default_validity_time(10),
  session_manager:set_validity_time(Domain, 2),
  session_manager:set_subdomain_validity_time(Domain, 5),
  ?assertEqual(2, session_manager:get_validity_time(Url)),
  ?assertEqual(5, session_manager:get_validity_time(Subdomain)),
  ?assertEqual(10, session_manager:get_validity_time(OtherDomain)).

