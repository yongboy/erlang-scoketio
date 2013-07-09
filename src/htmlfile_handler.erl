-module(htmlfile_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).
-define(HEARBEAT_TIMEOUT, socketio:get_env(heartbeat_timeout)*1000).

init({_Transport, http}, Req, State) ->
	{ok, Req, State}.

%% POST/Short Request
handle(Req, State) ->
	{Method, Req1} = cowboy_http_req:method(Req),
	{[_, _, _, BinarySessionId], Req2} = cowboy_http_req:path(Req1),
	SessionId = binary_to_list(BinarySessionId),
	case Method of
		'POST' ->
			handle_post(Req2, SessionId, State);
		'GET' ->
			handle_get(Req2, SessionId, State)
	end.

handle_get(Req, SessionId, _State) ->
	common_polling:set_timeout(SessionId, ?HEARBEAT_TIMEOUT),

	{ok, Reply} = cowboy_http_req:chunked_reply(200, [{<<"Content-Type">>, <<"text/html; charset=utf-8">>}], Req),
	cowboy_http_req:chunk("<html><body><script>var _ = function (msg) { parent.s._(msg, document); };</script>                                                                                                                                                                                                                  ", 
								Reply),
	session_server:cast({SessionId, self(), subscribe, htmlfile}),
	wait_data(SessionId, Reply),
	session_server:cast({SessionId, end_connect}),
	{ok, Req, undefined_state}.

wait_data(SessionId, Reply) ->
    receive
        {reply, first} ->
			cowboy_http_req:chunk(gen_output("1::"), Reply);
		{reply, Message} ->
			cowboy_http_req:chunk(gen_output(Message), Reply);
		_Any ->
			void
    end,
	
    wait_data(SessionId, Reply).

gen_output(String) ->
	DescList = io_lib:format("<script>_('~s');</script>", [String]),
	list_to_binary(DescList).

handle_post(Req, SessionId, _State) ->
	Result = case cowboy_http_req:body(Req) of
		{ok, Data, Req2} ->
			Msg = binary_to_list(Data),
			common_polling:do_post_msg({SessionId, Msg});
		{error, timeout} ->
			Req2 = Req,
			"1"
	end,
	{_, Req3} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Result), Req2),
	{ok, Req3, SessionId}.

terminate(_Req, _State) ->
	ok.