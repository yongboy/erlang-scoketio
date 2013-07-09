%% @author yongboy <yong.boy@gmail.com>
%% @copyright 2012 yongboy <yong.boy@gmail.com>
%% @doc socketio
-module(socketio_impl).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([behaviour_info/1]).

%%
%% API Functions
%%
%% @private
-spec behaviour_info(_)
		-> undefined | [{on_init, 1} | {on_connect, 2} | {on_message, 2} | {on_disconnect, 2} | {on_destroy, 1}].
behaviour_info(callbacks) ->  
    [
	 {on_init, 1},  
     {on_connect, 2},
	 {on_message, 2},
	 {on_disconnect, 2},
	 {on_destroy, 1}
	];

behaviour_info(_Other) ->  
    undefined.