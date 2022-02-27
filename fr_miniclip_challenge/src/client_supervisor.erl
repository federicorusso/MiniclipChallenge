%%%-------------------------------------------------------------------
%%% @author Federico Russo
%%% @copyright (C) 2022, ForTech
%%% @doc
%%%
%%% @end
%%% Created : 26. feb 2022 19:44
%%%-------------------------------------------------------------------
-module(client_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_socket/0]).

-define(PORT, 8888).
-define(MAX_LISTENERS, 25).
-define(MAX_RESTARTS, 60).
-define(MAX_RESTART_INTERVAL, 3600).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("Starting server on port ~p~n", [?PORT]),
  {ok, ListenSocket} = gen_tcp:listen(?PORT, [{active,once}, {packet,line}]),
  spawn_link(fun empty_listeners/0),
  {ok, {{simple_one_for_one, ?MAX_RESTARTS, ?MAX_RESTART_INTERVAL},
    [{socket,
      {sockserv_serv, start_link, [ListenSocket]},
      temporary, 1000, worker, [sockserv_serv]}
    ]}},

  %% Define child specification
  AChild = #{id => 'client',
    start => {'client', start_link, [ListenSocket]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => ['client']},

  %% Define strategy to start a new child
  {ok, {#{strategy => simple_one_for_one,
    intensity => 5,
    period => 30},
    [AChild]}
  }.

start_socket() ->
  supervisor:start_child(?MODULE, []).

empty_listeners() ->
  io:format("Creating ~p listeners...~n", [?MAX_LISTENERS]),
  [start_socket() || _ <- lists:seq(1,?MAX_LISTENERS)],
  ok.