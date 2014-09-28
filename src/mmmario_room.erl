%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% 部屋状態を表現するモジュール
%%% 1. idle状態 -> 部屋生成後、まだ最大収容人数に達していない場合の状態
%%% 2. pregame状態 -> 最大収容人数に達した後の遷移する状態。ここでゲーム開始前のチェックを行う
%%% 3. ongame状態 -> ゲーム中状態。ゲームの内部状態は別のモジュールで保持するので、ここでは突然接続が切れたプレイヤーの管理だけを行う。
%%% 4. postgame状態 -> ゲームが終わったら遷移する状態。ゲームの終了処理を行う。この状態の後、部屋プロセスはkillされる。
%%%
%%% またRoomはキャラクター位置などのゲーム情報もETSを使って保持する
%%% @end
%%% Created : 08. 9 2014 23:43
%%%-------------------------------------------------------------------
-module(mmmario_room).
-author("lycaon").

-behaviour(gen_fsm).

%% API
-export([
  start_link/0,
  stop/1,
  new_player/3,
  exit_player/2,
  ready_player/2,
  die_player/2,
  move_player/3,
  new_block/3,
  change_name/3
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

-include("mmmario_room.hrl").

%%--------------------------------------------------------------------
%% @doc
%% 部屋状態レコード
%% @end
%%--------------------------------------------------------------------
-record(roomstate, {
  ptid, % プレイヤー情報用ETSテーブルのID
  empid, % イベントマネージャPID
  pcount = 0, % 部屋にいる(はずの)プレイヤー数
  rcount = 0 % ゲーム開始直前のプレイヤー数(ゲーム開始以降この属性は固定になる)
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
  gen_fsm:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% 部屋停止
%% @end
%%--------------------------------------------------------------------
stop(RPid) ->
  gen_fsm:sync_send_all_state_event(RPid, stop).

%%--------------------------------------------------------------------
%% @doc
%% 新規プレイヤー参上
%% ここで既存の部屋の空き状態をチェックし、なければ新しく作成する
%% プレイヤーが入れられた部屋のUidを返す
%% @end
%%--------------------------------------------------------------------
new_player(RPid, PUid, Name) ->
  gen_fsm:send_event(RPid, {new_player, PUid, Name}).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーの離脱
%% @end
%%--------------------------------------------------------------------
exit_player(RPid, PUid) ->
  gen_fsm:send_all_state_event(RPid, {exit_player, PUid}).

%%--------------------------------------------------------------------
%% @doc
%% pregame状態で、プレイヤーが準備出来た場合この関数を呼ぶ
%% @end
%%--------------------------------------------------------------------
ready_player(RPid, PUid) ->
  gen_fsm:send_event(RPid, {ready_player, PUid}).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーが動いた
%% @end
%%--------------------------------------------------------------------
move_player(RPid, PUid, Rect) ->
  gen_fsm:send_event(RPid, {move_player, PUid, Rect}).

%%--------------------------------------------------------------------
%% @doc
%% 新しいブロックを生成
%% @end
%%--------------------------------------------------------------------
new_block(RPid, PUid, Rect) ->
  gen_fsm:send_event(RPid, {new_block, PUid, Rect}).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーがゲームで死んだ時に呼ぶ関数
%% @end
%%--------------------------------------------------------------------
die_player(RPid, PUid) ->
  gen_fsm:send_event(RPid, {die_player, PUid}).

%%--------------------------------------------------------------------
%% @doc
%% 名前変更
%% @end
%%--------------------------------------------------------------------
change_name(RPid, PUid, Name) ->
  gen_fsm:send_all_state_event(RPid, {change_name, PUid, Name}).

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
  PTid = ets:new(?SERVER, [set, protected, {keypos, #cinfo.uid}, {read_concurrency, true}, {write_concurrency, true}]),
  {ok, EMPid} = mmmario_room_event:start_link(),
  process_flag(trap_exit, true),
  {ok, idle, #roomstate{ptid = PTid, empid = EMPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 待ち状態。
%% プレイヤー追加イベントが来たら追加する
%% @end
%%--------------------------------------------------------------------
idle({new_player, PUid, Name}, State = #roomstate{ptid = PTid, empid = EMPid, pcount = PCount}) ->
  MaxPCount = application:get_env(mmmario, maxpcount, 2),
  NextPCount = PCount + 1,
  HandlerId = mmmario_room_event:add_handler(EMPid, PTid, PUid),
  ets:insert(PTid, #cinfo{uid = PUid, hid = HandlerId, name = Name}),
  if
    MaxPCount =:= NextPCount ->
      error_logger:info_msg("Room[~p] is full of players.~n", [self()]),
      mmmario_room_server:new_state(self(), pregame),
      mmmario_room_event:notice_ready(EMPid),
      {next_state, pregame, State#roomstate{pcount = NextPCount}};
    0 =< NextPCount andalso MaxPCount > NextPCount ->
      {next_state, idle, State#roomstate{pcount = NextPCount}};
    0 > NextPCount orelse MaxPCount < NextPCount ->
      error_logger:error_msg("NextPCount is odd.~n"),
      ok = mmmario_room_server:delete_room(self()),
      {stop, "NextPCount is odd.", State}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ゲーム開始前状態。
%% 全てのプレイヤーの準備ができたらゲーム状態に移る
%% @end
%%--------------------------------------------------------------------
pregame({ready_player, _PUid}, State = #roomstate{pcount = PCount, rcount = RCount}) ->
  NextRCount = RCount + 1,
  if
    PCount =:= NextRCount ->
      error_logger:info_msg("Room[~p] All players are ready to game!!~n", [self()]),
      mmmario_room_server:new_state(self(), ongame),
      {next_state, ongame, State#roomstate{rcount = NextRCount}};
    0 =< NextRCount andalso PCount > NextRCount ->
      {next_state, pregame, State#roomstate{rcount = NextRCount}};
    0 > NextRCount orelse PCount < NextRCount ->
      true = mmmario_room_server:delete_room(self()),
      {stop, "RCount is odd.", State}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ゲーム中状態。
%% 負けたプレイヤーに対応する
%% @end
%%--------------------------------------------------------------------
ongame({die_player, PUid}, State = #roomstate{ptid = PTid, pcount = PCount, empid = EMPid}) ->
  NextPCount = PCount - 1,
  IsKey = ets:member(PTid, PUid),
  if
    IsKey andalso 1 < NextPCount ->
      error_logger:info_msg("The player [~p] dies.~n", [PUid]),
      ets:update_element(PTid, PUid, {#cinfo.state, dead}),
      {next_state, ongame, State#roomstate{pcount = NextPCount}};
    IsKey andalso 1 == NextPCount ->
      ets:update_element(PTid, PUid, {#cinfo.state, dead}),
      AlivePUid = hd(hd(ets:match(PTid, #cinfo{state = alive, uid = '$1', _ = '_'}))),
      error_logger:info_msg("The player [~p] wins at the room [~p].~n", [AlivePUid, self()]),
      mmmario_room_event:notice_winner(EMPid, AlivePUid),
      mmmario_room_server:new_state(self(), postgame),
      {next_state, postgame, State#roomstate{pcount = NextPCount}};
    not IsKey -> {next_state, ongame, State};
    0 >= NextPCount ->
      ok = mmmario_room_server:delete_room(self()),
      {stop, "No body in the room.", State#roomstate{pcount = 0}}
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% プレイヤーが動いた！
%% ので全プレイヤーに通知を行う
%% @end
%%--------------------------------------------------------------------
ongame({move_player, PUid, Rect}, State = #roomstate{ptid = PTid, empid = EMPid}) ->
  ets:update_element(PTid, PUid, {#cinfo.rect, Rect}),
  mmmario_room_event:notice_rects(EMPid, PUid),
  {next_state, ongame, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 新しくブロックが生成された時によぶ。通知もする
%% @end
%%--------------------------------------------------------------------
ongame({new_block, PUid, Rect}, State = #roomstate{empid = EMPid}) ->
  mmmario_room_event:notice_new_block(EMPid, PUid, Rect),
  {next_state, ongame, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ゲーム終了状態。
%% 終了処理を行う。
%% @end
%%--------------------------------------------------------------------
postgame({_, _PUid}, State) ->
  {next_state, postgame, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% exit_playerイベントはあらゆる状態で処理する必要がある
%% @end
%%--------------------------------------------------------------------
handle_event({exit_player, PUid}, StateName, State = #roomstate{ptid = PTid, empid = EMPid, pcount = PCount}) ->
  NextPCount = PCount - 1,
  HandlerId = hd(hd(ets:match(PTid, #cinfo{uid = PUid, hid = '$1', _ = '_'}))),
  mmmario_room_event:delete_handler(EMPid, HandlerId),
  ets:match_delete(PTid, #cinfo{uid = PUid, _ = '_'}),
  if
    1 < NextPCount ->
      {next_state, StateName, State#roomstate{pcount = NextPCount}};
    ongame =:= StateName andalso 1 =:= NextPCount ->
      AlivePUid = hd(hd(ets:match(PTid, #cinfo{state = alive, uid = '$1', _ = '_'}))),
      error_logger:info_msg("The player [~p] wins at the room [~p].~n", [AlivePUid, self()]),
      mmmario_room_event:notice_winner(EMPid, AlivePUid),
      mmmario_room_server:new_state(self(), postgame),
      {next_state, postgame, State#roomstate{pcount = NextPCount}};
    0 >= NextPCount ->
      error_logger:error_msg("No body in the room[~p]~n", [self()]),
      ok = mmmario_room_server:delete_room(self()),
      {stop, "No body in the room.", State#roomstate{pcount = 0}}
  end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 名前変更イベント
%% @end
%%--------------------------------------------------------------------
handle_event({change_name, PUid, Name}, StateName, State = #roomstate{ptid = PTid}) ->
  ets:update_element(PTid, PUid, {#cinfo.name, Name}),
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% stopイベントの処理
%% @end
%%--------------------------------------------------------------------
handle_sync_event(stop, _From, _StateName, State) ->
  Reply = ok,
  {stop, "Stop room", Reply, State};

handle_sync_event(_Event, _From, StateName, State) ->
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
%% 終了時に呼ばれる。
%% @end
%%--------------------------------------------------------------------
terminate(Reason, StateName, _State) ->
  error_logger:warning_msg("The room[~p] is terminated with reason[~p]. (state is ~p)~n", [self(), Reason, StateName]),
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
%% @private
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
    {"Players", ets:match_object(StatusData#roomstate.ptid, #cinfo{_ = '_'})}]}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
