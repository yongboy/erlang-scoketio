%% @author {{author}}
%% @copyright {{year}} {{author}}
%% @doc {{appid}}.

-module({{appid}}_impl).
-behaviour(socketio_impl).
-define(NickMap, chat_tab).

-export([on_init/1, on_connect/2, on_disconnect/2, on_message/2, on_destroy/1]).

on_init(_Name) ->
	ets:new(?NickMap, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]).
on_destroy(_Name) ->
	void.

on_connect({SessionId, _MessageId, _Endpoint, OriMessage}, _SendFn) ->
	lager:debug("chat demo was called on_connect funtion with OriMsg : ~s and session id ~s ~n", [OriMessage, SessionId]).

on_disconnect({SessionId, _Endpoint, _SubMsgData}, SendFn) ->
	Nickname = get_nickname(SessionId),
	ets:delete(?NickMap, SessionId),
	SessionIds = get_sessions(),
	Type = "5",
	Announcement = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", Nickname ++ " disconnected"])),
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~s}]}", ["nicknames", get_format_nicknames()])),
	SendFn(Announcement, {SessionIds, Type}),
	SendFn(NicknameNotice, {SessionIds, Type}).

on_message({SessionId, Type, MessageId, Endpoint, Message}, SendFn) ->
	case string:len(MessageId) > 0  of
		true ->
			Ack = MessageId ++ "[false]",
			SendFn(Ack, ack);
		false -> ok			
	end,
	{_, D} = mochijson2:decode(Message),
	Key = proplists:get_value(<<"name">>, D),
	handle_event_name(Key, D, {SessionId, Type, Endpoint, Message, SendFn}).

%%
%% Local Functions
%%
handle_event_name(<<"nickname">>, Json, {SessionId, Type, _Endpoint, _Message, SendFn}) ->
	[NicknameBinary] = proplists:get_value(<<"args">>, Json),
	NickNameStr = lists:flatten(binary_to_list(NicknameBinary)),
	ets:insert(?NickMap, {SessionId, NickNameStr}),
	Welcome = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\"]}", ["announcement", NickNameStr ++ " connected"])),
	SessionIds = get_sessions(),
	SendFn(Welcome, {SessionIds, Type}),
	NicknameNotice = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[{~s}]}", ["nicknames", get_format_nicknames()])),
	SendFn(NicknameNotice, SessionIds);

handle_event_name(<<"user message">>, Json, {SessionId, _Type, _Endpoint, _Message, SendFn}) ->
	[MessageBinary] = proplists:get_value(<<"args">>, Json),
	MsgTxtStr = lists:flatten(binary_to_list(MessageBinary)),

	Nickname = get_nickname(SessionId),
	SessionIds = get_sessions(),	
	JsonMessage = lists:flatten(io_lib:format("{\"name\":\"~s\",\"args\":[\"~s\",\"~s\"]}", ["user message", Nickname, MsgTxtStr])),
	SendFn(JsonMessage, lists:delete(SessionId, SessionIds));
handle_event_name(<<_>>, _Json, {_SessionId, _Type, _Endpoint, Message, SendFn}) ->
	SendFn(Message, self).

get_sessions() ->
		ets:foldl(fun({OneSessionId, _}, Arrs) ->
					[OneSessionId|Arrs]
				end, [], ?NickMap).
get_nickname(SessionId) ->
	case ets:lookup(?NickMap, SessionId) of
		[] ->
			"undefined";
		[{_, Value}] ->
			Value
	end.
get_format_nicknames() ->
	NicknameList = ets:foldl(fun({_, Value}, Arrs) ->
											 Str = lists:flatten(io_lib:format("\"~s\":\"~s\",", [Value, Value])),
											 Arrs ++ [Str] end, [], ?NickMap),
	NickNameStr = lists:flatten(NicknameList),
	case string:len(NickNameStr) > 0 of
		true ->
			string:substr(NickNameStr, 1, string:len(NickNameStr)-1);
		false -> NickNameStr
	end.