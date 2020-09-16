-module(load_balancer).

-export([start/0]).

-define(INSTANCES, [
					{ "service-a", 5000 },
					{ "service-b", 5001 },
					{ "service-c", 5002 },
					{ "service-d", 5003 },
					{ "service-e", 5004 }
				   ]).


start() ->
	controller:start_link(?INSTANCES),
	server:start(6000).


