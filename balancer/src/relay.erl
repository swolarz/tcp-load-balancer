-module(relay).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(Client) ->
	{ok, Pid} = gen_server:start_link(?MODULE, Client, []),
	gen_tcp:controlling_process(Client, Pid).


init(CliSock) ->
	{ok, {CAddress, CPort}} = inet:peername(CliSock),
	{ok, {Address, Port}} = controller:resolve_instance(CAddress, CPort),
	{ok, SrvSock} = gen_tcp:connect(Address, Port, [binary, {packet, 0}, {active, true}]),
	{ok, {CliSock, SrvSock, nomillis}}.


handle_info({tcp, CliSock, Req}, {CliSock, SrvSock, ReqTimestamp}) ->
	ReqMillis = request_timestamp(ReqTimestamp),
	gen_tcp:send(SrvSock, Req),
	{noreply, {CliSock, SrvSock, ReqMillis}};

handle_info({tcp_closed, CliSock}, {CliSock, SrvSock, _ReqTimestamp}) ->
	gen_tcp:close(SrvSock),
	{stop, "client closed the connection", {CliSock, SrvSock, _ReqTimestamp}};

handle_info({tcp, SrvSock, Resp}, {CliSock, SrvSock, _ReqTimestamp}) ->
	gen_tcp:send(CliSock, Resp),
	{noreply, {CliSock, SrvSock, _ReqTimestamp}};

handle_info({tcp_closed, SrvSock}, {CliSock, SrvSock, _ReqTimestamp} = State) ->
	gen_tcp:shutdown(CliSock, read),
	{stop, "service response finished", State}.


request_timestamp(nomillis) ->
	os:system_time(millisecond);

request_timestamp(ReqTimestamp) ->
	ReqTimestamp.


handle_call(_Request, _From, State) ->
	{noreply, {}, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

