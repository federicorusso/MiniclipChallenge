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

-export([room_exists/1, enter_room/2, leave_room/2]).

-define(SERVER, ?MODULE).

-record(room_state, {name = undefined, users = []}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(RoomName) ->
  io:format("[START] Starting child for room with name ~s~n", [RoomName]),
  gen_server:start_link({local, list_to_atom("room_" ++ RoomName)}, ?MODULE, [RoomName], []).

init([RoomName]) ->
  io:format("[INIT] Starting child for room with name ~s~n", [RoomName]),
  {ok, #room_state{name = RoomName}}.

enter_room(RoomName, Username) ->
  gen_server:cast(whereis(list_to_atom("room_"++RoomName)), {enter, Username}).

leave_room(RoomName, Username) ->
  gen_server:cast(whereis(list_to_atom("room_"++RoomName)), {leave, Username}).

handle_call(_Request, _From, State = #room_state{}) ->
  {reply, ok, State}.

handle_cast({enter, Username}, State = #room_state{name = RoomName, users = Subscribers}) ->
  Member = lists:member(Username, Subscribers),
  if Member =:= false ->
    io:format("User ~s entered room ~s~n", [Username,RoomName]),
    {noreply, State#room_state{users = [Username | Subscribers]}};
    true ->
      io:format("User ~s already entered room ~s~n", [Username,RoomName]),
      {noreply, State}
  end;

handle_cast({leave, Username}, State = #room_state{name = RoomName, users = Subscribers}) ->
  Member = lists:member(Username, Subscribers),
  if Member =:= true ->
    io:format("User ~s left room ~s~n", [Username,RoomName]),
    %% The lists:delete could be done regardless, but the IF allows to log the different cases
    {noreply, State#room_state{users = lists:delete(Username, RoomName)}};
    true ->
      io:format("User ~s already not in room ~s~n", [Username,RoomName]),
      {noreply, State}
  end;

handle_cast({broadcast, Payload, Sender}, State = #room_state{users = Subscribers}) ->
  [ client:receive_message(User, Payload, Sender) || User <- Subscribers ],
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
  whereis(list_to_atom("room_" ++ RoomName)) =:= undefined.