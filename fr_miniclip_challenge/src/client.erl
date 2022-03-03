%%%-------------------------------------------------------------------
%%% @author Federico Russo
%%% @copyright (C) 2022, ForTech
%%% @doc
%%%
%%% @end
%%% Created : 26. feb 2022 19:11
%%%-------------------------------------------------------------------
-module(client).
-author("federico__russo").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% Client APIs
-export([receive_message/3, receive_message_internal/2]).


-define(SERVER, ?MODULE).
-define(SOCK(Msg), {tcp, _Port, Msg}).

-record(client_state, {socket = undefined, next_step = undefined, name = "", current_recipient = "", broadcast_room_name = "", received_message_count = 0, favourite_games = []}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init(Socket) ->
  State = #client_state{socket = Socket},
  gen_server:cast(self(), {accept}),
  {ok, State}.

%% Client APIs
receive_message(Username, Payload, Sender) ->
  gen_server:cast(whereis(list_to_atom("user_" ++ string:lowercase(Username))), {message, Payload, Sender}).

receive_message_internal(Username, Payload) ->
  gen_server:cast(whereis(list_to_atom("user_" ++ string:lowercase(Username))), {message_internal, Payload}).


%% Room APIs
list_rooms(Username) ->
  room_supervisor:list_rooms(Username).

create_room(RoomName, Username) ->
  room_supervisor:create_room(RoomName, Username).

enter_room(RoomName, Username) ->
  room:enter_room(RoomName, Username).

leave_room(RoomName, Username) ->
  room:leave_room(RoomName, Username).

broadcast_to_users_in_room(RoomName, Username, Payload) ->
  room:room_broadcast(RoomName, Username, Payload).

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #client_state{}) ->
  {reply, Reply :: term(), NewState :: #client_state{}} |
  {reply, Reply :: term(), NewState :: #client_state{}, timeout() | hibernate} |
  {noreply, NewState :: #client_state{}} |
  {noreply, NewState :: #client_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #client_state{}} |
  {stop, Reason :: term(), NewState :: #client_state{}}).

handle_call(_Request, From, CurrentState) ->
  %% Note to self: consider returning error instead - TODO: log entire request for debugging/analysis
  io:format("Received unexpected request from ~s~n", From),
  {reply, ok, CurrentState}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #client_state{}) ->
  {noreply, NewState :: #client_state{}} |
  {noreply, NewState :: #client_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #client_state{}}).

handle_cast({accept}, CurrentState) ->
  ListenSocket = CurrentState#client_state.socket,
  %%io:format("[DEBUG] Current socket is ~s ~n", [ListenSocket]),
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  client_supervisor:start_socket(),
  send(AcceptSocket, "What's your username?", []),
  %%io:format("[DEBUG] Switching to socket ~s ~n", [AcceptSocket]),
  {noreply, #client_state{socket=AcceptSocket, next_step = login}};

handle_cast(list, CurrentState = #client_state{name = SelfUser, favourite_games = Games, socket = Socket}) ->
  if Games =:= [] ->
    send(Socket, "User ~s has no favourite games (yet)~nInteract with the app using either of the following:  ~s", [SelfUser, implemented_operations()]),
    {noreply, CurrentState#client_state{next_step = general}};
    true ->
      send(Socket, "User ~s's favourite games are ~p~nInteract with the app using either of the following:  ~s", [SelfUser, Games, implemented_operations()]),
      {noreply, CurrentState#client_state{next_step = general}}
  end;

handle_cast({add_game, GameName}, CurrentState = #client_state{socket = Socket, favourite_games = Games}) ->
  %% Favourite games can be added asynchronously
  %% TODO: check if already inserted with lists:member(GameName, FavouriteGames)
  %% Make a dedicated function since guards don't support this directly
  %% TODO: Add RequesterUsername parameter and check if different from client_state.name --> users can only add favourite games to themselves
  _AlreadyAdded = lists:member(GameName, Games),
  %%io:format("[CAST] Adding ~s to favourite games ~n", [GameName]),
  send(Socket, "Added ~s to favourites", [GameName]),
  {noreply, CurrentState#client_state{favourite_games = [GameName | Games]}};
%%  if AlreadyAdded =:= false ->
%%    io:format("[CAST] Adding ~s to favourite games ~n", [GameName]),
%%    send(Socket, "Added ~s to favourites", [GameName]),
%%    {noreply, CurrentState#client_state{favourite_games = [GameName | Games]}};
%%    true ->
%%      send(Socket, "Game already added.~n~s", [GameName, format_impl_ops()]),
%%      {noreply, CurrentState}
%%  end;

handle_cast({message, Payload, SenderUsername}, CurrentState = #client_state{socket = Socket, received_message_count = Counter}) ->
  send(Socket, "Received message from user '~s' with payload: '~s' Total messages received: ~p", [SenderUsername, Payload, Counter + 1]),
  {noreply, CurrentState#client_state{received_message_count = Counter + 1}};

handle_cast({message_internal, Payload}, CurrentState = #client_state{socket = Socket}) ->
  send(Socket, "~s", [Payload]),
  {noreply, CurrentState};

handle_cast(_Request, State = #client_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #client_state{}) ->
  {noreply, NewState :: #client_state{}} |
  {noreply, NewState :: #client_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #client_state{}}).
handle_info(?SOCK(Str), CurrentState = #client_state{socket = Socket, next_step = login}) ->
  Name = string:lowercase(line(Str)),

  UsernameAvailable = username_available(Name),
  if UsernameAvailable =:= true ->
    io:format("User with name ~s logged in~n", [Name]),
    register(list_to_atom("user_" ++ Name), self()),
    send(Socket,"Welcome! Here's what you can do:\r~n ~s~n ~s~n ~s~n ~s~n ~s~n ~s~n",
      ["Enter 'room_list' to list existing rooms\r", "Enter 'room_create' to create a new room\r", "Enter 'room_enter' to join an existing room\r",
        "Enter 'room_leave' to leave an existing room\r", "Enter 'room_broadcast' to broadcast a message to all members of an existing room\r", "Enter 'message_user' to send a message to an active user"]),
    {noreply, #client_state{name = Name, next_step = general, socket = CurrentState#client_state.socket}};
    true ->
      io:format("Username ~s already taken, please choose another one~n", [Name]),
      send(CurrentState#client_state.socket, "The username specified is already in use; please choose a new one", []),
      {noreply, CurrentState}
  end;
handle_info(?SOCK(Str), CurrentState = #client_state{socket = Socket, name = Username, next_step = general}) ->
  Action = string:lowercase(line(Str)),
  case Action of
    "room_list" ->
      io:format("Requested room list~n"),
      list_rooms(Username),
      {noreply, CurrentState};
    "room_create" ->
      io:format("Requested room create~n"),
      send(Socket,"Enter name for the room to create: ", []),
      {noreply, CurrentState#client_state{next_step = create_room}};
    "room_enter" ->
      io:format("Requested room join~n"),
      send(Socket,"Enter name for the room to enter: ", []),
      {noreply, CurrentState#client_state{next_step = enter_room}};
    "room_leave" ->
      io:format("Requested room exit~n"),
      send(Socket,"Enter name for the room to leave: ", []),
      {noreply, CurrentState#client_state{next_step = leave_room}};
    "room_broadcast" ->
      io:format("Requested broadcast to room users~n"),
      send(Socket,"Enter room name for message broadcast: ", []),
      {noreply, CurrentState#client_state{next_step = broadcast_room}};
    "message_user" ->
      io:format("Requested message user~n"),
      send(Socket,"Enter recipient username: ", []),
      {noreply, CurrentState#client_state{next_step = message_user}};
    _ ->
      io:format("Action ~s requested is not currently managed~n", [Action]),
      send(Socket, "Action ~s requested is not currently managed\r~nInteract with the app using either of the following: ~s", [Action, implemented_operations()]),
      {noreply, CurrentState}
  end;

handle_info(?SOCK(Str), CurrentState = #client_state{socket = Socket, name = _Username, next_step = message_user}) ->
  Recipient = string:lowercase(line(Str)),
  UserExists = (whereis(list_to_atom("user_" ++ Recipient)) =/= undefined),
  if UserExists =:= true ->
    send(Socket, "Enter the message to send to user ~s: ", [Recipient]),
    {noreply, CurrentState#client_state{next_step = message_payload, current_recipient = Recipient}};
    true ->
      send(Socket, "User with name ~s is currently offline and cannot receive messages\r~nInteract with the app using either of the following: ~p", [Recipient, implemented_operations()]),
      {noreply, CurrentState#client_state{next_step = general}}
  end;

handle_info(?SOCK(Str), CurrentState = #client_state{socket = Socket, name = Username, current_recipient = Recipient, next_step = message_payload}) ->
  %% TODO: Find a way to only trim the trailing \n - Note to self: consider using "re"
  %%Payload = line(Str),
  Payload = Str,
  io:format("Sending message '~s' to recipient ~s from sender ~s (untrimmed: '~s')~n", [Payload, Recipient, Username, Str]),
  send(Socket, "Message sent!\r~nInteract with the app using either of the following: ~s", [implemented_operations()]),
  receive_message(Recipient, Payload, Username),
  {noreply, CurrentState#client_state{next_step = general, current_recipient = ""}};

handle_info(?SOCK(Str), CurrentState = #client_state{socket = _Socket, name = Username, next_step = create_room}) ->
  RoomName = string:lowercase(line(Str)),
  io:format("Requesting to create room ~s... ~n", [RoomName]),
  create_room(RoomName, Username),
  {noreply, CurrentState#client_state{next_step = general}};

handle_info(?SOCK(Str), CurrentState = #client_state{socket = _Socket, name = Username, next_step = enter_room}) ->
  RoomName = string:lowercase(line(Str)),
  io:format("Requesting to enter room ~s... ~n", [RoomName]),
  enter_room(RoomName, Username),
  {noreply, CurrentState#client_state{next_step = general}};

handle_info(?SOCK(Str), CurrentState = #client_state{socket = _Socket, name = Username, next_step = leave_room}) ->
  RoomName = string:lowercase(line(Str)),
  io:format("Requesting to leave room ~s... ~n", [RoomName]),
  leave_room(RoomName, Username),
  {noreply, CurrentState#client_state{next_step = general}};

handle_info(?SOCK(Str), CurrentState = #client_state{socket = Socket, next_step = broadcast_room}) ->
  RoomName = string:lowercase(line(Str)),
  io:format("Requesting payload to broadcast to users in room '~s'... ~n", [RoomName]),
  send(Socket,"Enter message for broadcast to users in room ~s: ", [RoomName]),
  {noreply, CurrentState#client_state{next_step = broadcast_payload, broadcast_room_name = RoomName}};

handle_info(?SOCK(Str), CurrentState = #client_state{socket = _Socket, name = Username, broadcast_room_name = RoomName, next_step = broadcast_payload}) ->
  Payload = Str,
  io:format("Broadcasting '~s' to all users in room '~s'... ~n", [Payload, RoomName]),
  broadcast_to_users_in_room(RoomName, Username, Payload),
  {noreply, CurrentState#client_state{next_step = general, broadcast_room_name = ""}};

handle_info(_Info, State = #client_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #client_state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #client_state{},
    Extra :: term()) ->
  {ok, NewState :: #client_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #client_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"\r~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

line(Str) ->
  %% Trim string in case a connection happens through telnet
  %% io:format("Trimming string '~s' ~n", [Str]),
  Len = string:length(Str),
  if Len > 0 ->
    hd(string:tokens(Str, "\r\n "));
    true ->
      ""
  end.

username_available(Username) ->
  whereis(list_to_atom("user_" ++ Username)) =:= undefined.

implemented_operations() ->
  Ops = ["room_list", "room_create", "room_enter", "room_leave", "room_broadcast", "message_user"],
  Res = lists:foldl(fun(X, Acc) -> X ++ ", " ++ Acc end, [], lists:reverse(Ops)),
  Len = string:length(Res),
  string:slice(Res, 0, Len-2).