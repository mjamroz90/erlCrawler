-module(session_manager_test).
-include_lib("eunit/include/eunit.hrl").

session_manager_test_() ->
  {setup,
    fun setup/0,
    fun cleanup/1,
    [
      fun subdomain_params_test/0
    ]
  }.

setup() ->
  application:start(session_manager).

cleanup(_) ->
  application:stop(session_manager).

subdomain_params_test() ->
  ?_test(
    begin
      Domain1 = "test.test",
      Params1 = {2, 1},
      session_manager:set_subdomain_params(Domain1, element(1, Params1), element(2, Params1)),

      Domain2 = "aaa.bbb",
      Params2 = {3, 4},
      session_manager:set_subdomain_params(Domain2, element(1, Params2), element(2, Params2)),

      ?assertEqual(Params2, session_manager:get_subdomain_params(Domain2)),
      ?assertEqual(Params1, session_manager:get_subdomain_params(Domain1))

    end
  ).

