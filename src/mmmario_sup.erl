%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 8 2014 15:45
%%%-------------------------------------------------------------------
-module(mmmario_sup).
-author("lycaon").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  PlayerSup = {mmmario_player_sup, {mmmario_player_sup, start_link, []},
    Restart, Shutdown, supervisor, [mmmario_player_sup]},

  WSServSup = {mmmario_wsserv_sup, {mmmario_wsserv_sup, start_link, []},
    Restart, Shutdown, supervisor, [mmmario_wsserv_sup]},

  EvtMgr = {{local, mmmario_game_event_handler}, {mmmario_game_event_handler, start_link, []},
    Restart, Shutdown, supervisor, [mmmario_event_handler]},

  RoomSup = {mmmario_room_sup, {mmmario_room_sup, start_link, []},
    Restart, Shutdown, supervisor, [mmmario_room_sup]},

  {ok, {SupFlags, [PlayerSup, WSServSup, EvtMgr, RoomSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
