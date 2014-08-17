%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 8 2014 13:53
%%%-------------------------------------------------------------------
-module(mmmario_player_sup).
-author("lycaon").

-behaviour(supervisor).

%% API
-export([start_link/0, start_player/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_player(WSServPid, Name) ->
  supervisor:start_child(?MODULE, [WSServPid, Name]). % simple_one_for_oneだとArgsの部分が自動で子に渡されるらしい

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 2000,
  Type = worker,

  Player = {mmmario_player, {mmmario_player, start_link, []},
    Restart, Shutdown, Type, [mmmario_player]},

  {ok, {SupFlags, [Player]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
