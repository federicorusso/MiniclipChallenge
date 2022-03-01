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
-export([introduce_self/1, receive_message/3, add_favourite_game/2, list_favourite_games/1]).

-export([login/1]).

-define(SERVER, ?MODULE).
-define(SOCK(Msg), {tcp, _Port, Msg}).

-record(client_state, {socket = undefined, next_step = undefined, name = "", received_message_count = 0, favourite_games = []}).

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

introduce_self(Username) ->
  gen_server:call(whereis(list_to_atom(Username)), {introduce}).

receive_message(Username, Payload, Sender) ->
  gen_server:call(whereis(list_to_atom(Username)), {message, Payload, Sender}).

add_favourite_game(Username, GameName) ->
  gen_server:cast(whereis(list_to_atom(Username)), {add_game, GameName}).

list_favourite_games(Username) ->
  io:format("Listing games for user ~s~n", [Username]),
  %%gen_server:call(whereis(list_to_atom(Username)), {list}).
  %%gen_server:cast(whereis(list_to_atom(Username)), list).
  gen_server:cast(whereis(list_to_atom(Username)), list).



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

handle_call({introduce}, _From, CurrentState) ->
  io:format("Hi, I'm ~s! ~n", [CurrentState#client_state.name]),
  {reply, "200 OK", CurrentState};
handle_call({message, Payload, SenderUsername}, _From, CurrentState) ->
  io:format("Received message from user ~s with payload: ~s ~n", [SenderUsername, Payload]),
  Counter = CurrentState#client_state.received_message_count + 1,
  {reply, "204 No Content", #client_state{name = CurrentState#client_state.name, socket=CurrentState#client_state.socket, received_message_count = Counter, favourite_games = CurrentState#client_state.favourite_games}};
handle_call({list}, _From, CurrentState) ->
  SelfUsername = CurrentState#client_state.name,
  io:format("User ~s's favourite games are: ~p~n", [SelfUsername, CurrentState#client_state.favourite_games]),
  {reply, "200 OK", CurrentState};

handle_call(list, _From, CurrentState = #client_state{next_step = general}) ->
  SelfUsername = CurrentState#client_state.name,
  io:format("User ~s's favourite games are: ~p~n", [SelfUsername, CurrentState#client_state.favourite_games]),
  {reply, "200 OK", CurrentState};

%%handle_call(game_list, _From, CurrentState = #client_state{next_step = general}) ->
%%  io:format("Requested game list~n"),
%%  {reply, ok, CurrentState#client_state{next_step = game_list}};

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
  send(AcceptSocket, "What's your username?~n", []),
  %%io:format("[DEBUG] Switching to socket ~s ~n", [AcceptSocket]),
  {noreply, #client_state{socket=AcceptSocket, next_step = login}};

handle_cast(list, CurrentState = #client_state{name = SelfUser, favourite_games = Games, socket = Socket}) ->
  if Games =:= [] ->
    send(Socket, "User ~s has no favourite games (yet)~nInteract with the app using either of the following:  ~p~n", [SelfUser, implemented_operations()]),
    {noreply, CurrentState#client_state{next_step = general}};
    true ->
      send(Socket, "User ~s's favourite games are ~p~nInteract with the app using either of the following:  ~p~n", [SelfUser, Games, implemented_operations()]),
      {noreply, CurrentState#client_state{next_step = general}}
      end;

handle_cast({add_game, GameName}, CurrentState = #client_state{socket = Socket, favourite_games = Games}) ->
  %% Favourite games can be added asynchronously
  %% TODO: check if already inserted with lists:member(GameName, FavouriteGames)
  %% Make a dedicated function since guards don't support this directly
  %% TODO: Add RequesterUsername parameter and check if different from client_state.name --> users can only add favourite games to themselves
  AlreadyAdded = lists:member(GameName, Games),
  %%io:format("[CAST] Adding ~s to favourite games ~n", [GameName]),
  send(Socket, "Added ~s to favourites~n", [GameName]),
  {noreply, CurrentState#client_state{favourite_games = [GameName | Games]}};
%%  if AlreadyAdded =:= false ->
%%    io:format("[CAST] Adding ~s to favourite games ~n", [GameName]),
%%    send(Socket, "Added ~s to favourites~n", [GameName]),
%%    {noreply, CurrentState#client_state{favourite_games = [GameName | Games]}};
%%    true ->
%%      send(Socket, "Game already added.~n~s~n", [GameName, format_impl_ops()]),
%%      {noreply, CurrentState}
%%  end;

handle_cast(_Request, State = #client_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #client_state{}) ->
  {noreply, NewState :: #client_state{}} |
  {noreply, NewState :: #client_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #client_state{}}).
handle_info(?SOCK(Str), CurrentState = #client_state{next_step = login}) ->
  Name = string:lowercase(line(Str)),

  UsernameAvailable = username_available(Name),
  if UsernameAvailable =:= true ->
    io:format("User with name ~s logged in~n", [Name]),
    AvailableOps = implemented_operations(),
    register(list_to_atom(Name), self()),
    send(CurrentState#client_state.socket, "Logged in! Interact with the app using either of the following:  ~p~n", [AvailableOps]),
    {noreply, #client_state{name = Name, next_step = general, socket = CurrentState#client_state.socket}};
    true ->
      io:format("Username ~s already taken, please choose another one~n", [Name]),
      send(CurrentState#client_state.socket, "The username specified is already in use; please choose a new one~n", []),
      {noreply, CurrentState}
  end;
handle_info(?SOCK(Str), CurrentState = #client_state{socket = Socket, name = Username, next_step = general}) ->
  Action = string:lowercase(line(Str)),
  case Action of
    "game_list" ->
      io:format("Requested game list for user ~s~n", [Username]),
      list_favourite_games(Username),
      {noreply, CurrentState#client_state{next_step = general}};
    "game_add" ->
      io:format("Requested game add~n"),
      send(Socket,"Enter game name to add to favourites: ~n", []),
      {noreply, CurrentState#client_state{next_step = game_add}};
    "room_create" ->
      io:format("Requested room create~n"),
      {noreply, CurrentState};
    "room_list" ->
      io:format("Requested room list~n"),
      {noreply, CurrentState};
    _ ->
      io:format("Action ~s requested is not currently managed~n", [Action]),
      send(Socket, "Action ~s requested is not currently managed~n", [Action]),
      {noreply, CurrentState}
  end;

handle_info(?SOCK(Str), CurrentState = #client_state{socket = _Socket, name = Username, next_step = game_add}) ->
  GameName = string:lowercase(line(Str)),
  io:format("Adding game ~s to favourites~n", [GameName]),
  add_favourite_game(Username, GameName),
  {noreply, CurrentState#client_state{next_step = general}};

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
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

line(Str) ->
  %% Trim string in case a connection happens through telnet
  %% io:format("Trimming string '~s' ~n", [Str]),
  hd(string:tokens(Str, "\r\n ")).

username_available(Username) ->
  whereis(list_to_atom(Username)) =:= undefined.

implemented_operations() ->
  ["game_list", "game_add", "room_list", "room_add"].

format_impl_ops() ->
  Res = io:format("Interact with the app using either of the following:  ~p~n", [implemented_operations()]),
  Res.