%%%-------------------------------------------------------------------
%%% @author Federico Russo
%%% @copyright (C) 2022, ForTech
%%% @doc
%%%
%%% @end
%%% Created : 19. feb 2022 15:59
%%%-------------------------------------------------------------------
-module(client).
-author("federico__russo").
-behavior(gen_server).

%% API
-export([start_link/1, send_message/2]).

start_link(UserName) ->
  gen_server:start_link({local, UserName}, ?MODULE, UserName, []).

handle_cast({message, Payload, SenderUsername}, _From, Subscribers) ->
  io:format("Received message '" ++ [Payload] ++ "' from user " ++ SenderUsername),
  {noreply, []}.

send_message(Payload, SenderUsername) ->
  gen_server:cast(SenderUsername, {message, Payload, SenderUsername}).