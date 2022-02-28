%%%-------------------------------------------------------------------
%%% @author feder
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. feb 2022 00:41
%%%-------------------------------------------------------------------
-module(connection_manager).
-author("feder").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([user_login/2, send_message_to_user/3, username_available/1, logged_users/0]).

-define(SERVER, ?MODULE).

-record(state, {names = [], records = []}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER},?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("Started module ~s (under supervisor) for user connection management~n", [?MODULE]),
  {ok, #state{}}.

user_login(Pid, Username) ->
  io:format("Logging in user ~s~n", [Username]),
  gen_server:call(self(), {add, Pid, Username}).

send_message_to_user(Payload, Recipient, Sender) ->
  gen_server:call(self(), {message, Recipient, Payload, Sender}).

username_available(Username) ->
  io:format("Checking availability of username ~s~n", [Username]),
  gen_server:call(self(), {available, Username}).

logged_users() ->
  gen_server:call(self(), {list}).

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({add, Pid, Username}, _From, State = #state{names = List, records = Users}) ->
  UserExists = lists:member(Username, List),
  if UserExists =:= false ->
    {reply, "200 - Added", #state{names = [Username | List], records = [{Username, Pid} | Users]}};
    true ->
      {reply, "404 - Invalid username", State}
  end;
handle_call({message, Username, Payload, Sender}, _From, State) ->
  Pid = get_pid_by_username(Username, State),
  if Pid == [] ->
    {reply, "404 - Invalid username", State};
    true ->
      client:receive_message(Pid, Payload, Sender),
      {reply, "200 OK", State}
  end;
handle_call({available, Username}, _From, State = #state{names = List}) ->
  UserExists = lists:member(Username, List),
  if UserExists =:= false ->
    {reply, available, State};
    true ->
      {reply, not_available, State}
  end;
handle_call({list}, _From, State = #state{names = List}) ->
  io:format("Logged users are ~p~n", [List]),
  {reply, "204 - No content", State};
handle_call(_Request, _From, State = #state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State = #state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_pid_by_username(Username, _State = #state{names = List, records = Users}) ->
  UserExists = lists:member(Username, List),
  if UserExists =:= false ->
    [];
    true ->
      [ Pid || {Name, Pid} <- Users, Name == Username ]
  end.