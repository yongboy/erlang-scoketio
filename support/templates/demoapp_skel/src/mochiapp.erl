%% @author {{author}}
%% @copyright {{year}} {{author}}
%% @doc {{appid}}.

-module({{appid}}).
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the {{appid}} server.
start() ->
	socketio:start(),
	endpoint_server:register("/chat", {{appid}}_impl).

%% @spec stop() -> ok
%% @doc Stop the {{appid}} server.
stop() ->
    application:stop(socketio).