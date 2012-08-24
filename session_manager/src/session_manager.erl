%% @doc Modul wspomagajacy.
-module(session_manager).

-export([get_validity_time/1]).
-export([insert/4, update/4, remove/1]).

%% @type url() = string()

%% @private
get_validity_time(_Domain) ->
	1000000000.

%% @spec insert(Url :: url(), MaxDepth :: integer(), MaxWidth :: integer(), _ValidityTime :: integer()) -> node()
%% @doc Przekazuje adres do przetworzenia dla scheduler'a.
insert(Url, MaxDepth, MaxWidth, _ValidityTime) ->
	Domain = reg:get_domain(Url),
	case application:get_env(erlCrawler,domain_manager_node) of
		{ok, Node} ->
			DestinationNode = rpc:call(Node, domain_dispatch_server, insert, [Domain]);
		undefined ->
			DestinationNode = domain_dispatch_server:insert(Domain)
	end,
	Params = common:stick_params({timestamp, 0},
				common:stick_params({width, MaxWidth},
					common:stick_params({depth, MaxDepth}, [])
				)
			),
	rpc:call(DestinationNode, scheduler, insert, [Url, Params]),
	DestinationNode.

%% @private
update(_Url, _MaxDepth, _MaxWidth, _ValidityTime) ->
	ok.

%% @private
remove(_Url) ->
	ok.


%tutaj chyba bedzie tez glowne api do statystyk


