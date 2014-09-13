%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 9 2014 11:45
%%%-------------------------------------------------------------------
-module(mmmario_room_test).
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
  {StateName, [{pcount, PCount}, {rcount, RCount}]}.

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
    fun add_new_player_to_room/1
  ],
  {foreach, fun setup_single_room/0, fun cleanup_single_room/1, Ins}.

%%--------------------------------------------------------------------
%% @doc
%% セットアップ関数
%% @end
%%--------------------------------------------------------------------
setup_single_room() ->
  mmmario_room_event_handler:start_link(),
  {ok, RPid} = mmmario_room:start_link(),
  RPid.

%%--------------------------------------------------------------------
%% @doc
%% クリーンアップ関数
%% @end
%%--------------------------------------------------------------------
cleanup_single_room(RPid) ->
  exit(RPid, normal).

%%--------------------------------------------------------------------
%% @doc
%% 新規プレイヤーを追加するのをテスト
%% @end
%%--------------------------------------------------------------------
add_new_player_to_room(RPid) ->
  mmmario_room:new_player(make_ref()),
  {StateName, Opts} = get_state(RPid),
  {pcount, PCount} = proplists:lookup(pcount, Opts),
  [?_assertEqual(idle, StateName),
    ?_assertEqual(1, PCount)].
