%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% 部屋状態を表現するモジュール
%%% 1. idle状態 -> 部屋生成後、まだ最大収容人数に達していない場合の状態
%%% 2. pregame状態 -> 最大収容人数に達した後の遷移する状態。ここでゲーム開始前のチェックを行う
%%% 3. ongame状態 -> ゲーム中状態。ゲームの内部状態は別のモジュールで保持するので、ここでは突然接続が切れたプレイヤーの管理だけを行う。
%%% 4. postgame状態 -> ゲームが終わったら遷移する状態。ゲームの終了処理を行う。この状態の後、部屋プロセスはkillされる。
%%% @end
%%% Created : 08. 9 2014 23:43
%%%-------------------------------------------------------------------
-module(mmmario_room).
-author("lycaon").

-behaviour(gen_fsm).

%% API
-export([
  start_link/0,
  new_player/1,
  exit_player/1,
  ready_player/1,
  die_player/1
]).

%% gen_fsm callbacks
-export([
  init/1,
  idle/2,
  pregame/2,
  ongame/2,
  postgame/2,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4,
  format_status/2
]).

-define(SERVER, ?MODULE).

-record(roomstate, {
  uid, % 部屋のUID {self(), make_ref()}
  pcount = 0, % 部屋にいる(はずの)プレイヤー数
  rcount = 0, % ゲーム開始直前のプレイヤー数(ゲーム開始以降この属性は固定になる)
  players = #{} % プレイヤーのUIDリスト, #{PUid => [{state, alive}]}
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% ルームを開始する
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% 新規プレイヤー参上
%% @end
%%--------------------------------------------------------------------
new_player(PUid) ->
  gen_fsm:send_event(?SERVER, {new_player, PUid}).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーの離脱
%% @end
%%--------------------------------------------------------------------
exit_player(PUid) ->
  gen_fsm:send_event(?SERVER, {exit_player, PUid}).

%%--------------------------------------------------------------------
%% @doc
%% pregame状態で、プレイヤーが準備出来た場合この関数を呼ぶ
%% @end
%%--------------------------------------------------------------------
ready_player(PUid) ->
  gen_fsm:send_event(?SERVER, {ready_player, PUid}).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーがゲームで死んだ時に呼ぶ関数
%% @end
%%--------------------------------------------------------------------
die_player(PUid) ->
  gen_fsm:send_event(?SERVER, {die_player, PUid}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 部屋を開始する。最初の状態はidle
%% @end
%%--------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  RUid = {self(), make_ref()},
  mmmario_room_event_handler:notify({new_room, RUid}),
  {ok, idle, #roomstate{uid = RUid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 待ち状態。
%% プレイヤー追加イベントが来たら追加する
%% @end
%%--------------------------------------------------------------------
idle({new_player, PUid}, State = #roomstate{uid = Ruid, pcount = PCount, players = PUids}) ->
  MaxPCount = application:get_env(mmmario, maxpcount, 6),
  NextPCount = PCount + 1,
  if
    NextPCount == MaxPCount ->
      error_logger:info_msg("Room[~p] is full of players.~n", [self()]),
      mmmario_room_event_handler:notify({full_room, Ruid}),
      {next_state, pregame, State#roomstate{pcount = NextPCount, players = maps:put(PUid, [{state, alive}], PUids)}};
    0 =< NextPCount andalso MaxPCount > NextPCount ->
      {next_state, idle, State#roomstate{pcount = NextPCount, players = maps:put(PUid, [{state, alive}], PUids)}};
    0 > NextPCount orelse MaxPCount < NextPCount ->
      error_logger:error_msg("NextPCount is odd.~n"),
      {stop, "NextPCount is odd.", State}
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 待ち状態。
%% プレイヤー離脱イベントが来たら削除する
%% @end
%%--------------------------------------------------------------------
idle({exit_player, PUid}, State = #roomstate{pcount = PCount, players = PUids}) ->
  NextPCount = PCount - 1,
  if
    0 =< NextPCount -> {next_state, idle, State#roomstate{pcount = NextPCount, players = maps:remove(PUid, PUids)}};
    0 > NextPCount -> {stop, "PCount is odd.", State#roomstate{pcount = NextPCount, players = #{}}}
  end.

%%--------------------------------------------------------------------
%% @doc
%% ゲーム開始前状態。
%% 全てのプレイヤーの準備ができたらゲーム状態に移る
%% 同期呼び出し
%% @end
%%--------------------------------------------------------------------
pregame({ready_player, _PUid}, State = #roomstate{uid = RUid, pcount = PCount, rcount = RCount}) ->
  NextRCount = RCount + 1,
  if
    PCount == NextRCount ->
      error_logger:info_msg("Room[~p] All players are ready to game!!~n", [RUid]),
      {next_state, ongame, State#roomstate{rcount = NextRCount}};
    0 =< NextRCount andalso PCount > NextRCount ->
      {next_state, pregame, State#roomstate{rcount = NextRCount}};
    0 > NextRCount orelse PCount < NextRCount -> {stop, "RCount is odd.", State}
  end;

%%--------------------------------------------------------------------
%% @doc
%% ゲーム開始前状態。
%% 突然のプレイヤー抜けに対応
%% @end
%%--------------------------------------------------------------------
pregame({exit_player, PUid}, State = #roomstate{uid = RUid, pcount = PCount, players = PUids}) ->
  NextPCount = PCount - 1,
  if
    0 < NextPCount ->
      error_logger:info_msg("The player [~p] is exit.~n", [PUid]),
      {next_state, pregame, State#roomstate{pcount = NextPCount, players = maps:remove(PUid, PUids)}}; % idleには戻らない
    0 >= NextPCount ->
      error_logger:error_msg("No body in the room.~n"),
      mmmario_room_event_handler:notify({abnormal_end, RUid}),
      {stop, "No body in the room.", State#roomstate{pcount = NextPCount, players = #{}}} % 誰もいなくなったらストップする
  end.

%%--------------------------------------------------------------------
%% @doc
%% ゲーム中状態。
%% 突然抜けたプレイヤーに対応する
%% @end
%%--------------------------------------------------------------------
ongame({exit_player, PUid}, State = #roomstate{uid = RUid, pcount = PCount, players = PUids}) ->
  NextPCount = PCount - 1,
  if
    0 < NextPCount ->
      error_logger:info_msg("The player [~p] is exit.~n", [PUid]),
      {next_state, ongame, State#roomstate{pcount = NextPCount, players = maps:remove(PUid, PUids)}};
    0 == NextPCount ->
      mmmario_room_event_handler:notify({abnormal_end, RUid}),
      {stop, "No body in the room.", State}
  end;

%%--------------------------------------------------------------------
%% @doc
%% ゲーム中状態。
%% 負けたプレイヤーに対応する
%% @end
%%--------------------------------------------------------------------
ongame({die_player, PUid}, State = #roomstate{uid = RUid, pcount = PCount, players = PUids}) ->
  NextPCount = PCount - 1,
  if
    1 < NextPCount ->
      error_logger:info_msg("The player [~p] dies.~n", [PUid]),
      {next_state, ongame, State#roomstate{pcount = NextPCount, players = maps:update(PUid, [{state, dead}], PUids)}};
    1 == NextPCount ->
      ExtractAlivePlayerFun = fun(K, V, Acc) when V =:= [{state, alive}] -> Acc = K end,
      AlivePUid = maps:fold(ExtractAlivePlayerFun, undefined, PUids),
      error_logger:info_msg("The player [~p] wins at the room [~p].~n", [AlivePUid, RUid]),
      {next_state, postgame, State#roomstate{pcount = NextPCount, players = maps:update(PUid, [{state, dead}], PUids)}};
    0 >= NextPCount ->
      mmmario_room_event_handler:notify({abnormal_end, RUid}),
      {stop, "No body in the room.", State#roomstate{pcount = 0, players = #{}}}
  end.

%%--------------------------------------------------------------------
%% @doc
%% ゲーム終了状態。
%% @end
%%--------------------------------------------------------------------
postgame({get_result, PUid}, State = #roomstate{pcount = PCount, players = PUids}) ->
  {next_state, postgame, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @doc
%% stopイベントの処理
%% @end
%%--------------------------------------------------------------------
handle_sync_event(stop, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%% @doc
%% 主にユニットテスト用…
%% @end
%%--------------------------------------------------------------------
format_status(normal, [PDict, StatusData]) ->
  io:format("PDict: ~p~n", [PDict]),
  io:format("StatusData: ~p~n", [StatusData]),
  [{data, [
    {"PCount", StatusData#roomstate.pcount},
    {"RCount", StatusData#roomstate.rcount},
    {"Players", StatusData#roomstate.players}]}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
