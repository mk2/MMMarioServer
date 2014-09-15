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
  start_link/2,
  stop/1,
  ready_player/1,
  move_player/2,
  change_player_name/2
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
-record(pstate, {
  uid, % プレイヤーのUID {pid(), #ref()}
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
  exit(PPid, normal).

%%--------------------------------------------------------------------
%% @doc
%% レディ状態を確認する
%% @end
%%--------------------------------------------------------------------
ready_player(PUid) ->
  error_logger:info_msg("~p~n", [PUid]),
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
%% 名前変更を行う
%% @end
%%--------------------------------------------------------------------
change_player_name(PUid, Name) ->
  gen_fsm:send_all_state_event(?PPID(PUid), {name, Name}).

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
  PUid = {self(), make_ref()},
  RPid = mmmario_room_server:new_player(PUid),
  process_flag(trap_exit, true),
  link(WSServPid),
  {ok, idle, #pstate{uid = PUid, wsservpid = WSServPid, name = Name, roompid = RPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% idle状態
%% readyイベントが来たら返答する
%% @end
%%--------------------------------------------------------------------
idle(ready, State = #pstate{roompid = RPid}) ->
  mmmario_room:ready_player(RPid, self()),
  {next_state, ongame, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% rects_changイベント
%% 他のプレイヤーのキャラクター位置が変わったので、クライアントへ通知する
%% @end
%%--------------------------------------------------------------------
ongame({rects_change, Rects}, State = #pstate{wsservpid = WSPid}) ->
  Text = string:join([rect_to_text(Rect) || Rect <- Rects], "|"),
  mmmario_wsserv:send(WSPid, Text),
  {next_state, ongame, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ゲーム状態
%% @end
%%--------------------------------------------------------------------
ongame({move, Rect}, State = #pstate{uid = PUid, roompid = RPid}) ->
  mmmario_room:move_player(RPid, PUid, Rect),
  {next_state, ongame, State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ゲーム状態で死亡
%% @end
%%--------------------------------------------------------------------
ongame(die, State) ->
  {next_state, postgame, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 名前書き換え
%% いつでも受け取る
%% @end
%%--------------------------------------------------------------------
handle_event({name, Name}, SName, S) ->
  {next_state, SName, S#pstate{name = Name}}.

handle_sync_event(_Event, _From, SName, S) ->
  Reply = ok,
  {reply, Reply, SName, S}.

handle_info(_Info, SName, S) ->
  {next_state, SName, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 終了処理関数
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _SName, #pstate{uid = PUid, roompid = RPid}) ->
  io:format("terminating player with: ~p~n", [Reason]),
  mmmario_room:exit_player(RPid, ?PPID(PUid)),
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
    {"Name", StatusData#pstate.name},
    {"Uid", StatusData#pstate.uid}]}].

%%%===================================================================
%%% Internal functions
%%%===================================================================
