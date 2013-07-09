%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio.
-module(socketio_decode).
-define(PATTERN, "(\\d):(\\d+\\+)?:(/[^:]*)?:?(.*)?").
-export([decode/1]).

%%
%% API Functions
%%
%% @spec decode(Msg) -> {[Type, MessageId, Endpoint, Data]} | {[]}
%% @doc decode the message, then return splied part
decode(Msg) ->
	case decode_structure(Msg) of
		[] ->
			{[]};
		[Type, MessageId, Endpoint, Data] ->
			{[Type, MessageId, Endpoint, Data]};
		_ ->
			{[]}
	end.

%%
%% Local Functions
%%
decode_structure(Msg) ->
	Result = re:run(Msg, ?PATTERN, [{capture, all_but_first, list}]),
	case Result of
		{match, Captured} ->
			Captured;
		nomatch ->
			[]
	end.