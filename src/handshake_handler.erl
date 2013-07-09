-module(handshake_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, State) ->
	{ok, Req, State}.

handle(Req, State) ->
	{Method, _} = cowboy_http_req:method(Req),
	{Tokens, _} = cowboy_http_req:path(Req),
	case Method of
		'POST' ->
			{ok, Req3} = do_post(Tokens, Req);
		'GET' ->
			{ok, Req2} = cowboy_http_req:set_resp_header(<<"Connection">>, <<"keep-alive">>, Req),
			{ok, Req3} = do_request(Tokens, Req2);
		_ ->
			Req3 = Req,
			lager:debug("not allowed request here ... with Req ~p~n", [Req])
	end,
	{ok, Req3, State}.

%%
%% Local Functions
%%
do_post([], Req) ->
	cowboy_http_req:reply(404, Req).

do_request([<<"socket.io">>, <<"1">>], Req) ->
	SessionId = uuid_server:gen(),
	session_server:register(SessionId),
	common_polling:set_timeout(SessionId),
	Msg = io_lib:format("~s:~p:~p:~s", [SessionId, socketio:get_env(heartbeat_timeout), socketio:get_env(close_timeout), socketio:get_env(allow_transports)]),
	OutputVal = list_to_binary(Msg),
	cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}], OutputVal, Req);

do_request(_, Req) ->
	cowboy_http_req:reply(404, Req).

terminate(_Req, _State) ->
	ok.