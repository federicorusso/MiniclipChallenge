%%%-------------------------------------------------------------------
%%% @author Federico Russo
%%% @copyright (C) 2022, ForTech
%%% @doc
%%%
%%% @end
%%% Created : 19. feb 2022 15:58
%%%-------------------------------------------------------------------
-module(client_supervisor).
-author("federico__russo").

%% API
-export([broadcast_message/3, send_message_to_user/3]).

broadcast_message(Message, Recipients, Sender) ->
  [Usr:send_message(Message, Sender) || Usr <- Recipients].

send_message_to_user(Message, Recipient, Sender) ->
  broadcast_message(Message, [Recipient], Sender).