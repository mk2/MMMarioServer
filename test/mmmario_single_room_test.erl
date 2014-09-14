%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 9 2014 11:45
%%%-------------------------------------------------------------------
-module(mmmario_single_room_test).
-author("lycaon").

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

%%====================================================================
%% ユーティリティ関数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% 指定したルームの状態を返す
%% 標準のsys:get_stateはformat_statusを無視するのでこれを作るしかなかった…
%% @end
%%--------------------------------------------------------------------
get_state(RPid) ->
  {status, RPid, _, [_, _, _, _, [{header, _}, {data, SysData}, {data, PData}]]} = sys:get_status(RPid),
  {"StateName", StateName} = proplists:lookup("StateName", SysData),
  {"PCount", PCount} = proplists:lookup("PCount", PData),
  {"RCount", RCount} = proplists:lookup("RCount", PData),
  {"Players", Players} = proplists:lookup("Players", PData),
  {StateName, [{pcount, PCount}, {rcount, RCount}, {players, Players}]}.

%%====================================================================
%% single_room_test用テスト
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% テストジェネレータ関数
%% @end
%%--------------------------------------------------------------------
single_room_test_() ->
  Ins = [
    fun add_new_player_to_room/1,
    fun max_player_to_room/1,
    fun exit_player_from_max_room/1,
    fun exit_player_from_not_max_room/1,
    fun winner_for_max_room_without_exit_player/1
  ],
  {foreach, fun setup_single_room/0, fun cleanup_single_room/1, Ins}.

%%--------------------------------------------------------------------
%% @doc
%% セットアップ関数
%% @end
%%--------------------------------------------------------------------
setup_single_room() ->
  {ok, _} = mmmario_room_event_handler:start_link(),
  {ok, RPid} = mmmario_room:start_link(),
  RPid.

%%--------------------------------------------------------------------
%% @doc
%% クリーンアップ関数
%% @end
%%--------------------------------------------------------------------
cleanup_single_room(RPid) ->
  exit(RPid, normal),
  mmmario_room_event_handler:stop().

%%--------------------------------------------------------------------
%% @doc
%% 新規プレイヤーを追加するをテスト
%% @end
%%--------------------------------------------------------------------
add_new_player_to_room(RPid) ->
  mmmario_room:new_player(make_ref()),
  {StateName, Opts} = get_state(RPid),
  {pcount, PCount} = proplists:lookup(pcount, Opts),
  [?_assertEqual(idle, StateName),
    ?_assertEqual(1, PCount)].

%%--------------------------------------------------------------------
%% @doc
%% 最大プレイヤーを追加してpregame状態になることをテスト
%% @end
%%--------------------------------------------------------------------
max_player_to_room(RPid) ->
  MaxPCount = application:get_env(mmmario, maxpcount, 6),
  {InitialStateName, InitialOpts} = get_state(RPid),
  {pcount, InitialPCount} = proplists:lookup(pcount, InitialOpts),
  [mmmario_room:new_player(make_ref()) || _ <- lists:seq(1, MaxPCount)],
  {PostStateName, PostOpts} = get_state(RPid),
  {pcount, PostPCount} = proplists:lookup(pcount, PostOpts),
  [?_assertEqual(idle, InitialStateName),
    ?_assertEqual(0, InitialPCount),
    ?_assertEqual(pregame, PostStateName),
    ?_assertEqual(6, PostPCount)].

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーを部屋から離脱させるテスト
%% MAXまで入れてから離脱させるのでidle状態には戻らない
%% @end
%%--------------------------------------------------------------------
exit_player_from_max_room(RPid) ->
  MaxPCount = application:get_env(mmmario, maxpcount, 6),
  {InitialStateName, InitialOpts} = get_state(RPid),
  {pcount, InitialPCount} = proplists:lookup(pcount, InitialOpts),
  PUids = [make_ref() || _ <- lists:seq(1, MaxPCount)],
  [mmmario_room:new_player(PUid) || PUid <- PUids],
  {MaxStateName, MaxOpts} = get_state(RPid),
  {pcount, MaxPCount1} = proplists:lookup(pcount, MaxOpts),
  % 1人ずつMaxPCount分離脱させる
  [mmmario_room:exit_player(PUid) || PUid <- PUids],
  {PostStateName, PostOpts} = get_state(RPid),
  {pcount, PostPCount} = proplists:lookup(pcount, PostOpts),
  [?_assertEqual(idle, InitialStateName),
    ?_assertEqual(0, InitialPCount),
    ?_assertEqual(pregame, MaxStateName),
    ?_assertEqual(MaxPCount, MaxPCount1),
    ?_assertEqual(pregame, PostStateName),
    ?_assertEqual(0, PostPCount)].

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーを部屋から離脱させるテスト
%% MAXまで入れずに離脱させるのでidle状態に戻る
%% @end
%%--------------------------------------------------------------------
exit_player_from_not_max_room(RPid) ->
  MaxPCount = 5,
  {InitialStateName, InitialOpts} = get_state(RPid),
  {pcount, InitialPCount} = proplists:lookup(pcount, InitialOpts),
  PUids = [make_ref() || _ <- lists:seq(1, MaxPCount)],
  [mmmario_room:new_player(PUid) || PUid <- PUids],
  {MaxStateName, MaxOpts} = get_state(RPid),
  {pcount, MaxPCount1} = proplists:lookup(pcount, MaxOpts),
  % 1人ずつMaxPCount分離脱させる
  [mmmario_room:exit_player(PUid) || PUid <- PUids],
  {PostStateName, PostOpts} = get_state(RPid),
  {pcount, PostPCount} = proplists:lookup(pcount, PostOpts),
  [?_assertEqual(idle, InitialStateName),
    ?_assertEqual(0, InitialPCount),
    ?_assertEqual(idle, MaxStateName),
    ?_assertEqual(MaxPCount, MaxPCount1),
    ?_assertEqual(idle, PostStateName),
    ?_assertEqual(0, PostPCount)].

%%--------------------------------------------------------------------
%% @doc
%% 満室状態で勝者を決める。
%% 途中離脱者はいないものとする
%% @end
%%--------------------------------------------------------------------
winner_for_max_room_without_exit_player(RPid) ->
  MaxPCount = application:get_env(mmmario, maxpcount, 6),
  PUids = [make_ref() || _ <- lists:seq(1, MaxPCount)],
  [mmmario_room:new_player(PUid) || PUid <- PUids],
  {PregameStateName, _} = get_state(RPid),
  [mmmario_room:ready_player(PUid) || PUid <- PUids],
  {OngameStateName, OngameOpts} = get_state(RPid),
  {rcount, OngameRCount} = proplists:lookup(rcount, OngameOpts),
  [mmmario_room:die_player(PUid) || PUid <- lists:droplast(PUids)], % リストの最後のプレイヤーを勝者とする
  {PostgameStateName, PostgameOpts} = get_state(RPid),
  {pcount, PostgamePCount} = proplists:lookup(pcount, PostgameOpts),
  [?_assertEqual(pregame, PregameStateName),
    ?_assertEqual(MaxPCount, OngameRCount),
    ?_assertEqual(ongame, OngameStateName),
    ?_assertEqual(postgame, PostgameStateName),
    ?_assertEqual(1, PostgamePCount)].
