%% Author: Administrator
%% Created: 2012-10-8
%% Description: TODO: Add description to flash_security_handler
-module(flash_security_handler).
-behaviour(cowboy_protocol).
-define(FLASH_REQ, <<"<policy-file-request/>\0">>).
-define(FLASH_FILE, <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>">>).
-record(state, {
	socket :: inet:socket(),
	transport :: module(),
	timeout :: timeout()
}).
-export([start_link/4]).
-export([init/4]).

%%
%% API Functions
%%
-spec init(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

%%
%% Local Functions
%%
-spec start_link(pid(), inet:socket(), module(), any()) -> ok.
init(_ListenerPid, Socket, Transport, Opts) ->
	Timeout = proplists:get_value(timeout, Opts, 5000),
    recv(#state{socket=Socket, transport=Transport, timeout=Timeout}),
    ok.

-spec recv(#state{}) -> ok.
recv(State = #state{socket=Socket, transport=Transport, timeout=Timeout})->
 	case Transport:recv(Socket, 0, Timeout) of
	    {ok, ?FLASH_REQ}->
			Transport:send(Socket, ?FLASH_FILE);
	    _Any ->
			ok
    end,
	terminate(State).

-spec terminate(#state{}) -> ok.
terminate(#state{socket=Socket, transport=Transport}) ->
	Transport:close(Socket),
	ok.