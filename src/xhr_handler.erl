-module(xhr_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, info/3, terminate/2]).
-define(HEARBEAT_INTERVAL, socketio:get_env(heartbeat_interval)*1000).
-define(HEARBEAT_TIMEOUT, socketio:get_env(heartbeat_timeout)*1000).

-spec init({_Transport, http}, Req, _State) ->
		  {ok, Req, string()} | {loop, Req, tuple(), integer(), hibernate}.
init({_Transport, http}, Req, _State) ->
	{Method, Req1} = cowboy_http_req:method(Req),
	{[_, _, _, BinarySessionId], Req2} = cowboy_http_req:path(Req1),
	SessionId = binary_to_list(BinarySessionId),
	case Method of
		'POST' ->
			{ok, Req2, SessionId};
		'GET' ->
			common_polling:set_timeout(SessionId, ?HEARBEAT_TIMEOUT),
			session_server:cast({SessionId, self(), subscribe, common_polling}),
			TimeoutRef = erlang:send_after(?HEARBEAT_INTERVAL, self(), timeout),
			{loop, Req2, {SessionId, TimeoutRef}, ?HEARBEAT_INTERVAL + 1000, hibernate}
	end.

%% @doc just handle post Request
-spec handle(Req, _SessionId) -> {ok, Req, undefined_state}.
handle(Req, SessionId) ->
	Result = case cowboy_http_req:body(Req) of
		{ok, Data, Req2} ->
			Msg = binary_to_list(Data),
			common_polling:do_post_msg({SessionId, Msg});
		{error, timeout} ->
			Req2 = Req,
			"1"
	end,
	{_, Req3} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Result), Req2),
	{ok, Req3, undefined_state}.

%% @doc receive messages from session_server
-spec info(_, Req, _) -> {ok, Req, undefined_state}.
info({reply, first}, Req, State) ->
    output("1::", Req, State);
info(timeout, Req, State) ->
	output("8::", Req, State);
info({reply, Message}, Req, State) ->
     output(Message, Req, State);
info(Any, Req, State) ->
	lager:debug("got unwanted message is ~p~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", [Any]),
    {loop, Req, State, hibernate}.

output(Message, Req, _State = {SessionId, TimeoutRef}) ->
	session_server:cast({SessionId, end_connect}),
	erlang:cancel_timer(TimeoutRef),
	{ok, Req2} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Message), Req),
	{ok, Req2, undefined_state}.

terminate(_Req, _State) ->
	ok.