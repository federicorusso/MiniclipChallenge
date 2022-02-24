%%%-------------------------------------------------------------------
%%% @author Federico Russo
%%% @copyright (C) 2022, ForTech
%%% @doc
%%%
%%% @end
%%% Created : 19. feb 2022 13:24
%%%-------------------------------------------------------------------
-module(room_with_pid).
-author("federico__russo").
-behavior(gen_server).

%% API
-export([start_link/1, init/1, user_enter/2, user_exit/2, list_users/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {name = "", subscribers = []}).

start_link(RoomName) ->
  gen_server:start_link(?MODULE, RoomName, []).
%%gen_server:start_link({local, RoomName}, ?MODULE, [], []).


init(RoomName) ->
  io:format("Started child~n"),
  {ok, #state{name = RoomName, subscribers = []}}.
%%io:format("Started child with Pid " ++ self() ++ "~n~p"),

user_enter(Pid, Username) ->
  gen_server:call(Pid, {enter, Username}).

user_exit(Pid, Username) ->
  gen_server:call(Pid, {exit, Username}).

list_users(Pid) ->
  gen_server:call(Pid, list_users).

handle_call({enter, Username}, _From, CurrentState) ->
  Subscribers = CurrentState#state.subscribers,
  UserSubscribed = user_subscribed(Username, Subscribers),
  if UserSubscribed =:= false ->
    {reply, "200 - Subscribed", #state{name = CurrentState#state.name, subscribers = [Username | Subscribers]}};
    true ->
      {reply, "409 - Already subscribed", CurrentState}
  end;
handle_call({exit, Username}, _From, CurrentState) ->
  Subscribers = CurrentState#state.subscribers,
  UserSubscribed = user_subscribed(Username, Subscribers),
  if UserSubscribed =:= true ->
    {reply, "200 - Unsubscribed", #state{name = CurrentState#state.name, subscribers = lists:delete(Username, Subscribers)}};
    true ->
      {reply, "400 - Not subscribed", CurrentState}
  end;
handle_call(list_users, _From, CurrentState) ->
  {reply, CurrentState#state.subscribers, CurrentState};
handle_call(_, _From, CurrentState) ->
  {reply, "Invalid request received, ignored", CurrentState}.

handle_cast(Request, CurrentState) ->
  io:format("Received async request: ~p~n", [Request]),
  {noreply, CurrentState}.

handle_info(_Info, CurrentState) ->
  {noreply, CurrentState}.

terminate(_Reason, CurrentState) ->
  ok.

%% Added for OTP consistency but not required
code_change(_OldVsn, CurrentState, _Extra) ->
  {ok, CurrentState}.

user_subscribed(Username, Subscribers) ->
  lists:member(Username, Subscribers).