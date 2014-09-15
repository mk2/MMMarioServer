%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 9 2014 15:56
%%%-------------------------------------------------------------------
-module(mmmario_player_test).
-author("lycaon").

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% ユーティリティ関数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% 指定したルームの状態を返す
%% 標準のsys:get_stateはformat_statusを無視するのでこれを作るしかなかった…
%% @end
%%--------------------------------------------------------------------
get_state(PPid) ->
  {status, PPid, _, [_, _, _, _, [{header, _}, {data, SysData}, {data, PData}]]} = sys:get_status(PPid),
  {"StateName", StateName} = proplists:lookup("StateName", SysData),
  {"Name", Name} = proplists:lookup("Name", PData),
  {StateName, [{name, Name}]}.

%%--------------------------------------------------------------------
%% @doc
%% フェイクのWSServのプロセスを生成する
%% @end
%%--------------------------------------------------------------------
spawnFakeWSSrv() ->
  spawn(fun Me() -> receive Any -> io:format("~p~n", [Any]), Me() end end).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーを生成
%% @end
%%--------------------------------------------------------------------
spawnPlayer() ->
  {ok, PPid} = mmmario_player:start_link(spawnFakeWSSrv(), make_ref()),
  PPid.


%%====================================================================
%% プレイヤーテスト
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% テストジェネレータ関数
%% @end
%%--------------------------------------------------------------------
player_test_() ->
  Ins = [
    fun add_player_to_room/1
  ],
  {foreach, fun setup_misc/0, fun cleanup_misc/1, Ins}.

%%--------------------------------------------------------------------
%% @doc
%% セットアップ関数
%% @end
%%--------------------------------------------------------------------
setup_misc() ->
  mmmario_room_sup:start_link(),
  mmmario_room_server:start_link(),
  mmmario_player_sup:start_link().

%%--------------------------------------------------------------------
%% @doc
%% クリーンアップ関数
%% @end
%%--------------------------------------------------------------------
cleanup_misc(_) ->
  mmmario_player_sup:stop(),
  mmmario_room_server:stop(),
  mmmario_room_sup:stop().

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーの状態がidleでないことをチェック
%% @end
%%--------------------------------------------------------------------
add_player_to_room(_WSPidSpawnFun) ->
  % まず5人
  PPids1 = [spawnPlayer() || _ <- lists:seq(1, 5)],
  PStatusList1 = [Status || {Status, _} <- [get_state(PPid) || PPid <- PPids1]],
  IsIdle = fun(State) -> idle =:= State end,
  [mmmario_player:stop(PPid) || PPid <- PPids1],
  % 次6人
  PPids2 = [spawnPlayer() || _ <- lists:seq(1, 6)],
  PStatusList2 = [Status || {Status, _} <- [get_state(PPid) || PPid <- PPids2]],
  IsOngame = fun(State) -> ongame =:= State end,
  [mmmario_player:stop(PPid) || PPid <- PPids2],
  [?_assert(lists:all(IsIdle, PStatusList1)),
    ?_assert(lists:all(IsOngame, PStatusList2)),
    ?_assertEqual(idle, hd(PStatusList1)),
    ?_assertEqual(ongame, hd(PStatusList2))].

