%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 9 2014 13:46
%%%-------------------------------------------------------------------
-module(mmmario_room_server_test).
-author("lycaon").

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% room server用テスト
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% テストジェネレータ関数
%% @end
%%--------------------------------------------------------------------
room_server_test_() ->
  Ins = [
    fun new_player_to_server/1
  ],
  {foreach, fun setup_room_server/0, fun cleanup_room_server/1, Ins}.

%%--------------------------------------------------------------------
%% @doc
%% セットアップ関数
%% @end
%%--------------------------------------------------------------------
setup_room_server() ->
  mmmario_room_sup:start_link(),
  {ok, SPid} = mmmario_room_server:start_link(),
  SPid.

%%--------------------------------------------------------------------
%% @doc
%% クリーンアップ関数
%% @end
%%--------------------------------------------------------------------
cleanup_room_server(SPid) ->
  exit(SPid, normal).

%%--------------------------------------------------------------------
%% @doc
%% 新規プレイヤー振り分けテスト
%% @end
%%--------------------------------------------------------------------
new_player_to_server(_SPid) ->
  PUids = [make_ref() || _ <- lists:seq(1, 10)],
  [mmmario_room_server:new_player(PUid, "Test") || PUid <- PUids],
  RoomCount = mmmario_room_server:all_room_count(),
  [?_assertEqual(2, RoomCount)].