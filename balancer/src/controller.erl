-module(controller).
-behaviour(gen_server).

-export([start_link/1, resolve_instance/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).


start_link(Instances) ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, Instances, []).


resolve_instance(Address, Port) ->
	Instance = gen_server:call({global, ?MODULE}, {resolve, Address, Port}),
	io:format("Resolved instance: ~p~n", [Instance]),
	{ok, Instance}.


init(Instances) ->
	io:format("Available instances: ~p~n", [Instances]),
	{ok, {Instances, []}}.


handle_call({resolve, Address, Port}, _From, {[], Waiting}) ->
	io:format("Resolving instance for client: ~p:~p~n", [Address, Port]),
	io:format("Round robin next round!~n"),
	[Instance | Instances] = lists:reverse(Waiting),
	{reply, Instance, {Instances, [Instance]}};

handle_call({resolve, Address, Port}, _From, {[Instance | Instances], Waiting}) ->
	io:format("Resolving instance for client: ~p:~p~n", [Address, Port]),
	{reply, Instance, {Instances, [Instance | Waiting]}}.


handle_cast(_Request, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


