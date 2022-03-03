%%%-------------------------------------------------------------------
%%% @author feder
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(room).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-export([room_exists/1, enter_room/2, leave_room/2, room_broadcast/3]).
-export([list_users/2]).

-define(SERVER, ?MODULE).

-record(room_state, {name = undefined, users = []}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(RoomName) ->
  io:format("Starting child for room with name ~s~n", [RoomName]),
  gen_server:start_link({local, list_to_atom("room_" ++ string:lowercase(RoomName))}, ?MODULE, [RoomName], []).

init([RoomName]) ->
  io:format("Starting child for room with name ~s~n", [RoomName]),
  {ok, #room_state{name = RoomName}}.

%% For testing only
list_users(RoomName, Requester) ->
  RoomExists = room_exists(RoomName),
  if RoomExists =:= true ->
    gen_server:cast(whereis(list_to_atom("room_" ++ string:lowercase(RoomName))), {users});
    true ->
      client:receive_message_internal(Requester, "Room does not exist")
  end.

enter_room(RoomName, Requester) ->
  io:format("Enter request from user ~s~n", [Requester]),
  RoomExists = room_exists(RoomName),
  if RoomExists =:= true ->
    gen_server:cast(whereis(list_to_atom("room_"++RoomName)), {enter, Requester});
    true ->
      client:receive_message_internal(Requester, "Room does not exist")
  end.

leave_room(RoomName, Requester) ->
  io:format("Leave request from user ~s~n", [Requester]),
  RoomExists = room_exists(RoomName),
  if RoomExists =:= true ->
    gen_server:cast(whereis(list_to_atom("room_" ++ string:lowercase(RoomName))), {leave, Requester});
    true ->
      client:receive_message_internal(Requester, "Room does not exist")
  end.


room_broadcast(RoomName, Requester, Payload) ->
  io:format("Broadcast request from user ~s to room ~s~n", [Requester, string:lowercase(RoomName)]),
  RoomExists = room_exists(RoomName),
  if RoomExists =:= true ->
    gen_server:cast(whereis(list_to_atom("room_" ++ string:lowercase(RoomName))), {broadcast, Payload, Requester});
    true ->
      client:receive_message_internal(Requester, "Room does not exist")
  end.

handle_call(_Request, _From, State = #room_state{}) ->
  {reply, ok, State}.

handle_cast({enter, Username}, State = #room_state{name = RoomName, users = Subscribers}) ->
  Member = lists:member(Username, Subscribers),
  if Member =:= false ->
    io:format("User ~s entered room ~s~n", [Username,RoomName]),
    client:receive_message_internal(Username, "Joined room!"),
    {noreply, State#room_state{users = [Username | Subscribers]}};
    true ->
      io:format("User ~s already entered room ~s~n", [Username,RoomName]),
      client:receive_message_internal(Username, "You have already joined the room!"),
      {noreply, State}
  end;

handle_cast({leave, Username}, State = #room_state{name = RoomName, users = Subscribers}) ->
  Member = lists:member(Username, Subscribers),
  if Member =:= true ->
    io:format("User ~s left room ~s~n", [Username,RoomName]),
    client:receive_message_internal(Username, "Left room"),
    %% The lists:delete could be done regardless, but the IF allows to log the different cases
    {noreply, State#room_state{users = lists:delete(Username, Subscribers)}};
    true ->
      io:format("User ~s already not in room ~s~n", [Username,RoomName]),
      client:receive_message_internal(Username, "You are not a member of this room!"),
      {noreply, State}
  end;

handle_cast({broadcast, Payload, Sender}, State = #room_state{name = RoomName, users = Subscribers}) ->
  if Subscribers =:= [] ->
    io:format("No members of room '~s' to receive message '~s' from user ~s~n", [RoomName, Payload, Sender]),
    client:receive_message_internal(Sender, "No members in room specified to forward message to!"),
    {noreply, State};
    true ->
      [ client:receive_message(User, Payload, Sender) || User <- Subscribers ],
      {noreply, State}
  end;

handle_cast({users}, State = #room_state{name = RoomName, users = Subscribers}) ->
  io:format("Users in room ~s are: ~p~n", [RoomName, Subscribers]),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State = #room_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #room_state{}) ->
  ok.

code_change(_OldVsn, State = #room_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

room_exists(RoomName) ->
  whereis(list_to_atom("room_" ++ RoomName)) =/= undefined.