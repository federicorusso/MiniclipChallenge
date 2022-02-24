%%%-------------------------------------------------------------------
%%% @author feder
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. feb 2022 17:20
%%%-------------------------------------------------------------------
-module(room_supervisor).
-author("feder").

-behaviour(supervisor).
-behaviour(gen_server).
-record(room, {name, roomPid}).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([create_room/2,enter_room/3, exit_room/3, list_rooms/1, broadcast_message/4]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  AChild = #{id => 'rm1',
    start => {'room', start_link, ["MainRoom"]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => ['room']},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_room(RoomName, CurrentState) ->
  %% Start a new room process
  ChildSpec = #{id => 'rm1',
    start => {'room', start_link, [RoomName]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => ['room']},
  {ok, NewRoomPid} = supervisor:start_child(roomSupervisor, ChildSpec),
  [ #room{name = RoomName, roomPid = NewRoomPid} | CurrentState ].

enter_room(RoomName, Requester, CurrentState) ->
  RoomExists = room_exists(RoomName, CurrentState),
  if RoomExists =:= true ->
    RoomPid = get_room_pid_by_name(RoomName, CurrentState),
    room:user_enter(RoomPid, Requester);
    true ->
      %% Out of scope - Possible strategy: create the room and enter it
      ok
  end.

exit_room(RoomName, Requester, CurrentState) ->
  RoomExists = room_exists(RoomName, CurrentState),
  if RoomExists =:= true ->
    RoomPid = get_room_pid_by_name(RoomName, CurrentState),
    room:user_exit(RoomPid, Requester);
    true ->
      %% Out of scope - Possible strategy: notify the user
      ok
  end.

list_rooms(CurrentState) ->
  %% For every room record in Current State extract the name using a list coprehension
  get_room_names(CurrentState).

broadcast_message(RoomName, Message, Requester, CurrentState) ->
  RoomExists = room_exists(RoomName, CurrentState),
  if RoomExists =:= true ->
    RoomPid = get_room_pid_by_name(RoomName, CurrentState),
    clientSupervisor:broadcast_message(Message, room:list_users(RoomPid), Requester);
    true ->
      %% Out of scope - Chosen strategy: notify the user
      LogMessage = "Cannot forward message '" ++ Message ++ "' to room with name '" ++RoomName ++ "' - room does not exist",
      clientSupervisor:broadcast_message(LogMessage, Requester, "SystemLog")
  end.

handle_call({enter, RoomName, Requester}, _From, CurrentState) ->
  RoomExists = room_exists(RoomName, CurrentState),
  if RoomExists =:= true ->
    RoomPid = get_room_pid_by_name(RoomName, CurrentState),
    room:user_enter(RoomPid, Requester);
    true ->
      %% Out of scope - Possible strategy: create the room and enter it
      ok
  end;
handle_call({exit, RoomName, Requester}, _From, CurrentState) ->
  RoomExists = room_exists(RoomName, CurrentState),
  if RoomExists =:= true ->
    RoomPid = get_room_pid_by_name(RoomName, CurrentState),
    room:user_exit(RoomPid, Requester);
    true ->
      %% Out of scope - Possible strategy: notify the user
      ok
  end;
handle_call(list, _From, CurrentState) ->
  get_room_names(CurrentState);
handle_call({broadcast, RoomName, Message, Requester}, _From, CurrentState) ->
  RoomExists = room_exists(RoomName, CurrentState),
  if RoomExists =:= true ->
    RoomPid = get_room_pid_by_name(RoomName, CurrentState),
    clientSupervisor:broadcast_message(Message, room:list_users(RoomPid), Requester);
    true ->
      %% Out of scope - Chosen strategy: notify the user
      LogMessage = "Cannot forward message '" ++ Message ++ "' to room with name '" ++ RoomName ++ "' - room does not exist",
      clientSupervisor:broadcast_message(LogMessage, Requester, "SystemLog")
  end.

%% Old version, check if/how necessary
%%loop(CurrentState) ->
%%  receive
%%    {From, create, RoomName} ->
%%      loop(create_room(RoomName, CurrentState));
%%    {From, enter, RoomName, Requester} ->
%%      room:user_enter(RoomName, Requester),
%%      loop(CurrentState);
%%    {From, exit, RoomName, Requester} ->
%%      room:user_exit(RoomName, Requester),
%%      loop(CurrentState);
%%    {From, list, Requester} ->
%%      CurrentState#roomSupState.rooms,
%%      loop(CurrentState);
%%    {From, message, Message, RoomName, Requester} ->
%%      clientSupervisor:broadcast_message(Message, room:list_users(RoomName), Requester),
%%      loop(CurrentState)
%%  end.

room_exists(RoomName, CurrentState) ->
  ExistingRooms = get_room_names(CurrentState),
  lists:member(RoomName, ExistingRooms).

get_room_names(CurrentState) ->
  [ R#room.name || R <- CurrentState ].

get_room_pid_by_name(RoomName, CurrentState) ->
  [ R#room.roomPid || R <- CurrentState, R#room.name =:= RoomName ].

