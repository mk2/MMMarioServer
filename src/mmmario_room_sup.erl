%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 9 2014 23:44
%%%-------------------------------------------------------------------
-module(mmmario_room_sup).
-author("lycaon").

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  stop/0,
  start_room/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% スーパーバイザを開始
%% @end
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% 停止
%% @end
%%--------------------------------------------------------------------
stop() ->
  exit(whereis(?SERVER), normal).

%%--------------------------------------------------------------------
%% @doc
%% ルームを開始する
%% @end
%%--------------------------------------------------------------------
start_room() ->
  supervisor:start_child(?SERVER, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 2000,
  Type = worker,

  Room = {mmmario_room, {mmmario_room, start_link, []},
    Restart, Shutdown, Type, [mmmario_room]},

  {ok, {SupFlags, [Room]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
