-module(server).

-export([start/1]).


start(Port) ->
	{ok, Sock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, true}]),
	io:format("Started load balancer service on port 6000~n"),
	server_loop(Sock).


server_loop(SrvSock) ->
	{ok, Sock} = gen_tcp:accept(SrvSock),
	handle_request(Sock),
	server_loop(SrvSock).


handle_request(Sock) ->
	relay:start_link(Sock).

