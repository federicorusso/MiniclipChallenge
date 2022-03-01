%%%-------------------------------------------------------------------
%%% @author feder
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. mar 2022 22:45
%%%-------------------------------------------------------------------
-module(room_supervisor).
-author("feder").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([create_room/2, list_rooms/1]).

-define(SERVER, ?MODULE).
-define(PERSISTENCE_FILE, "/src/RoomPersistenceLayer.txt").

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  io:format("Starting room supervisor~n"),
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
  SupFlags = #{strategy => simple_one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  AChild = #{id => 'room',
    start => {'room', start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => ['room']},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_room(RoomName, RequesterUsername) ->
  RoomExists = room_exists(RoomName),
  if RoomExists =:= false ->
    _AChild = #{id => 'room',
      start => {'room', start_link, [RoomName]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => ['room']},
    %% TODO - Check child creation
    supervisor:start_child(?MODULE, [RoomName]),
    update_persistence_layer(RoomName),
    client:receive_message(RequesterUsername, "Room created", ?MODULE);
    true ->
      io:format("Room with name ~s already exists - consider joining it!~n", [RoomName]),
      client:receive_message(RequesterUsername, "Room already exists - consider joining it!~n", ?MODULE)
  end.

list_rooms(RequesterUsername) ->
  {ok, CWD} = file:get_cwd(),
  FileName = CWD ++ ?PERSISTENCE_FILE,
  {ok, Content} = file:read_file(FileName),
  Rooms = string:tokens(binary_to_list(Content), ","),
  %%io:format("Rooms existing: ~p~n", [Rooms]),
  client:receive_message(RequesterUsername, Rooms, ?MODULE),
  Rooms.

%% Do not export
update_persistence_layer(RoomName) ->
  {ok, CWD} = file:get_cwd(),
  FileName = CWD ++ ?PERSISTENCE_FILE,

  {ok, Content} = file:read_file(FileName),
  Rooms = string:tokens(binary_to_list(Content), ","),
  io:format("~p~n", [Rooms]),

  RoomExists = lists:member(RoomName, Rooms),
  if RoomExists =:= false ->
    io:format("Adding room ~s to persistence layer~n", [RoomName]),
    {ok, FileStream} = file:open(FileName, [append]),
    io:format(FileStream, "~s,", [RoomName]);
    true ->
      io:format("Room with name ~s already exists~n", [RoomName])
  end.


room_exists(RoomName) ->
  whereis(list_to_atom("room_" ++ RoomName)) =/= undefined.