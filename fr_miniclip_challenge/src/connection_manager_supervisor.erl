%%%-------------------------------------------------------------------
%%% @author feder
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(connection_manager_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
  AChild = #{id => 'connection_manager',
    start => {'connection_manager', start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => ['connection_manager']},

  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    [AChild]}
  }.