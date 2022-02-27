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

-define(SERVER, ?MODULE).
-define(SOCK(Msg), {tcp, _Port, Msg}).

-record(client_state, {socket = undefined, name = "", received_message_count = 0, favourite_games = []}).

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
  %%io:format("Started child for user with name ~s~n", [Username]),
  State = #client_state{socket = Socket},
  gen_server:cast(self(), {accept}),
  {ok, State}.

introduce_self(Pid) ->
  gen_server:call(Pid, {introduce}).

receive_message(Pid, Payload, Sender) ->
  gen_server:call(Pid, {message, Payload, Sender}).

add_favourite_game(Pid, GameName) ->
  gen_server:cast(Pid, {add_game, GameName}).

list_favourite_games(Pid) ->
  gen_server:call(Pid, {list}).



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
  {noreply, #client_state{socket=AcceptSocket}};

handle_cast({add_game, GameName}, CurrentState) ->
  %% Favourite games can be added asynchronously
  %% TODO: check if already inserted with lists:member(GameName, FavouriteGames)
  %% Make a dedicated function since guards don't support this directly
  %% TODO: Add RequesterUsername parameter and check if different from client_state.name --> users can only add favourite games to themselves
  io:format("Adding ~s to favourite games ~n", [GameName]),
  Favourites = [GameName | CurrentState#client_state.favourite_games],
  {noreply, #client_state{name = CurrentState#client_state.name, socket=CurrentState#client_state.socket, received_message_count = CurrentState#client_state.received_message_count, favourite_games = Favourites}};
handle_cast(_Request, State = #client_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #client_state{}) ->
  {noreply, NewState :: #client_state{}} |
  {noreply, NewState :: #client_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #client_state{}}).
handle_info(?SOCK(Str), CurrentState) ->
  Name = line(Str),
  io:format("User with name ~s logged in~n", [Name]),
  %%io:format("[DEBUG] Current socket is ~s ~n", [Socket]),
  {noreply, #client_state{name=Name, socket=CurrentState#client_state.socket}};
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