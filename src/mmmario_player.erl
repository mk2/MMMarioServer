%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 8 2014 11:42
%%%-------------------------------------------------------------------
-module(mmmario_player).
-author("lycaon").

-behaviour(gen_fsm).

%% API
-export([
  puid/1,
  start_link/2,
  stop/1,
  ready_player/1,
  move_player/2,
  move_other_players/2,
  die_player/1,
  new_block/2,
  new_block_from_others/2,
  change_player_name/2,
  win_player/1
]).
%% gen_fsm callbacks
-export([
  init/1,
  idle/2,
  ongame/2,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4,
  format_status/2
]).

-define(SERVER, ?MODULE).
-define(PPID(PUid), element(1, PUid)).

-include("mmmario_game_type.hrl").

%%--------------------------------------------------------------------
%% @doc
%% プレイヤー状態
%% @end
%%--------------------------------------------------------------------
-record(playerstate, {
  uid, % プレイヤーのUID {pid(), Name}
  name, % プレイヤーの名前
  roompid, % roomのPid
  wsservpid, % wsservのPid
  pos = {0, 0}, % キャラクターの位置
  ltime = 0 % 生存時間
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% PPidからPUidを取得
%% @end
%%--------------------------------------------------------------------
puid(PPid) ->
  gen_fsm:sync_send_all_state_event(PPid, puid).

%%--------------------------------------------------------------------
%% @doc
%% 開始メソッド
%% WSServPidはその名の通り、mmmario_wsservのpid
%% @end
%%--------------------------------------------------------------------
start_link(WSServPid, Name) ->
  gen_fsm:start_link(?MODULE, [WSServPid, Name], []).

%%--------------------------------------------------------------------
%% @doc
%% 通常終了
%% @end
%%--------------------------------------------------------------------
stop(PPid) ->
  gen_fsm:sync_send_all_state_event(PPid, exit).

%%--------------------------------------------------------------------
%% @doc
%% レディ状態を確認する
%% @end
%%--------------------------------------------------------------------
ready_player(PUid) ->
  error_logger:info_msg("ready: ~p~n", [PUid]),
  gen_fsm:send_event(?PPID(PUid), ready).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーを動かす
%% @end
%%--------------------------------------------------------------------
move_player(PUid, Rect) ->
  gen_fsm:send_event(?PPID(PUid), {move, Rect}).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤー以外の位置を動かす
%% @end
%%--------------------------------------------------------------------
move_other_players(PUid, NamedRects) ->
  gen_fsm:send_event(?PPID(PUid), {move_others, NamedRects}).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤー死亡
%% @end
%%--------------------------------------------------------------------
die_player(PUid) ->
  gen_fsm:send_event(?PPID(PUid), die).

%%--------------------------------------------------------------------
%% @doc
%% 新しいブロック生成
%% @end
%%--------------------------------------------------------------------
new_block(PUid, Rect) ->
  gen_fsm:send_event(?PPID(PUid), {new_block, Rect}).

%%--------------------------------------------------------------------
%% @doc
%% 他のプレイヤーが新しくブロック生成
%% @end
%%--------------------------------------------------------------------
new_block_from_others(PUid, Rect) ->
  gen_fsm:send_event(?PPID(PUid), {new_block_from_others, Rect}).

%%--------------------------------------------------------------------
%% @doc
%% 名前変更を行う
%% @end
%%--------------------------------------------------------------------
change_player_name(PUid, Name) ->
  gen_fsm:send_all_state_event(?PPID(PUid), {name, Name}).

%%--------------------------------------------------------------------
%% @doc
%% 勝利通知を行う
%% @end
%%--------------------------------------------------------------------
win_player(PUid) ->
  gen_fsm:send_event(?PPID(PUid), win).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 初期化
%% 初期化後はidle状態にしてプレイヤーからのイベントを待つ
%% @end
%%--------------------------------------------------------------------
init([WSServPid, Name]) ->
  PUid = {self(), Name},
  RPid = mmmario_room_server:new_player(PUid),
  process_flag(trap_exit, true),
  link(WSServPid),
  {ok, idle, #playerstate{uid = PUid, wsservpid = WSServPid, name = Name, roompid = RPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% idle状態
%% readyイベントが来たら返答する
%% @end
%%--------------------------------------------------------------------
idle(ready, State = #playerstate{roompid = RPid}) ->
  mmmario_room:ready_player(RPid, self()),
  {next_state, ongame, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ゲーム状態
%% キャラクターの移動を受け取る
%% @end
%%--------------------------------------------------------------------
ongame({move, Rect}, State = #playerstate{uid = PUid, roompid = RPid}) ->
  mmmario_room:move_player(RPid, PUid, Rect),
  {next_state, ongame, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 自分以外のキャラを動かす
%% @end
%%--------------------------------------------------------------------
ongame({move_others, NamedRects}, State = #playerstate{wsservpid = WSSrvPid}) ->
  Data = "REC " ++ string:join(namedrects_to_text(NamedRects), " "),
  mmmario_wsserv:send(WSSrvPid, Data),
  {next_state, ongame, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 新しいブロック生成通知
%% @end
%%--------------------------------------------------------------------
ongame({new_block, Rect}, State = #playerstate{uid = PUid, roompid = RPid}) ->
  mmmario_room:new_block(RPid, PUid, Rect),
  {next_state, ongame, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 勝利通知
%% @end
%%--------------------------------------------------------------------
ongame(win, State = #playerstate{wsservpid = WSSrvPid}) ->
  mmmario_wsserv:send(WSSrvPid, "WIN"),
  {next_state, postgame, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 他のプレイヤーが生成したブロックをクライアントに通知する
%% @end
%%--------------------------------------------------------------------
ongame({new_block_from_others, Rect}, State = #playerstate{wsservpid = WSSrvPid}) ->
  Data = "BLK " ++ rect_to_text(Rect),
  mmmario_wsserv:send(WSSrvPid, Data),
  {next_state, ongame, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ゲーム状態で死亡
%% @end
%%--------------------------------------------------------------------
ongame(die, State = #playerstate{uid = PUid, roompid = RPid}) ->
  mmmario_room:die_player(RPid, PUid),
  {next_state, postgame, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 名前書き換え
%% いつでも受け取る
%% @end
%%--------------------------------------------------------------------
handle_event({name, Name}, SName, S) ->
  {next_state, SName, S#playerstate{name = Name}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 終了
%% @end
%%--------------------------------------------------------------------
handle_sync_event(exit, _From, _SName, State) ->
  Reply = ok,
  {stop, "Exit", Reply, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% PPidからPUidを取得
%% @end
%%--------------------------------------------------------------------
handle_sync_event(puid, _From, SName, State = #playerstate{uid = PUid}) ->
  Reply = PUid,
  {reply, Reply, SName, State};

handle_sync_event(_Event, _From, SName, State) ->
  Reply = ok,
  {reply, Reply, SName, State}.

handle_info(_Info, SName, S) ->
  {next_state, SName, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 終了処理関数
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _SName, #playerstate{uid = PUid, roompid = RPid}) ->
  error_logger:warning_msg("terminating player with: ~p~n", [Reason]),
  mmmario_room:exit_player(RPid, PUid),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, SName, S, _Extra) ->
  {ok, SName, S}.

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
    {"Name", StatusData#playerstate.name},
    {"Uid", StatusData#playerstate.uid}]}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
