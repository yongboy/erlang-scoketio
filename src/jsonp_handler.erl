-module(jsonp_handler).
-extends(xhr_handler).
-export([init/3, handle/2, info/3, terminate/2]).

init({_Transport, http}, Req, _State) ->
	?BASE_MODULE:init({_Transport, http}, Req, _State).

%% @doc just handle post Request
-spec handle(Req, _SessionId) -> {ok, Req, undefined_state}.
handle(Req, SessionId) ->
	%% TODO IE8下面发送的Unicode字符有待处理
	Binary = list_to_binary(get_post_value(<<"d">>, Req)),
	OriMsg = binary_to_list(Binary),
	Msg2 = string:substr(OriMsg, 2, string:len(OriMsg)-2),
	Msg = re:replace(Msg2, "\\\\+", "", [global]),
	Result = common_polling:do_post_msg({SessionId, Msg}),
	{_, Req2} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain; charset=utf-8">>}], list_to_binary(Result), Req),
	{ok, Req2, undefined_state}.

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

terminate(_Req, _State) ->
	ok.

%%
%% Local Functions
%%
output(Message, Req, _State = {Room, SessionId, TimeoutRef}) ->
	{I, Req1} = cowboy_http_req:qs_val(<<"i">>, Req),
	session_server:cast({SessionId, end_connect}),
	erlang:cancel_timer(TimeoutRef),
	DescList = io_lib:format("io.j[~s]('~s');", [I, Message]),
	{ok, Req2} = cowboy_http_req:reply(200, [
			{<<"Content-Type">>, 
			<<"application/x-javascript; charset=utf-8">>},
			{<<"X-XSS-Protection">>, <<"0">>}, 
			{<<"Connection">>, <<"keep-alive">>}
		], list_to_binary(DescList), Req1),
	{ok, Req2, undefined_state}.

get_post_values(Req) ->
    {Method, _} = cowboy_http_req:method(Req),
    get_post_values(Method, Req).

get_post_values('POST', Req) ->
    {Vals, _} = cowboy_http_req:body_qs(Req),
    Vals;
get_post_values(_, _) ->
    undefined.

get_post_value(Name, Req) ->
    PostVals = get_post_values(Req),
    extract_post_value(Name, PostVals).

extract_post_value(_, undefined) ->
    undefined;
extract_post_value(Name, PostVals) ->
    Matches = [X || X <- PostVals, Name =:= element(1,X)],
    process_post_value(Matches).

process_post_value([]) ->
    undefined;
process_post_value(Vals) ->
    {_, Result} = lists:unzip(Vals),
    Result.