-module(load_balancer).

-export([start/0]).

-define(INSTANCES, [
					{ "service", 5000 },
					{ "service", 5001 },
					{ "service", 5002 },
					{ "service", 5003 },
					{ "service", 5004 }
				   ]).


start() ->
	init_instances_observer(),
	start_server(6000).


init_instances_observer() ->
	%% Todo setup ETS and save there info about instances
	%% Spawn a linked process to monitor the instances
	io:format("Available instances: ~p~n", [?INSTANCES]).


start_server(Port) ->
	{ok, Sock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, true}]),
	io:format("Started load balancer service on port 6000~n"),
	server_loop(Sock).


server_loop(SrvSock) ->
	{ok, Sock} = gen_tcp:accept(SrvSock),
	%% Todo spawn a linked process to handle request
	handle_request(Sock),
	server_loop(SrvSock).


handle_request(CliSock) ->
	{ok, Client} = inet:peername(CliSock),
	{ok, {Address, Port}} = resolve_instance(Client),
	{ok, SrvSock} = gen_tcp:connect(Address, Port, [binary, {packet, 0}, {active, true}]),
	forward_request(CliSock, SrvSock).


forward_request(CliSock, SrvSock) ->
	receive
		{tcp, CliSock, Req} ->
			gen_tcp:send(SrvSock, Req),
			forward_request(CliSock, SrvSock);
		{tcp_closed, CliSock} ->
			gen_tcp:close(SrvSock);
		{tcp, SrvSock, Resp} ->
			gen_tcp:send(CliSock, Resp),
			forward_request(CliSock, SrvSock);
		{tcp_closed, SrvSock} ->
			gen_tcp:shutdown(CliSock, read)
	end.


resolve_instance(Client) ->
	io:format("Resolving instance for client: ~p~n", [Client]),
	[Instance | _] = ?INSTANCES,
	{ok, Instance}.


