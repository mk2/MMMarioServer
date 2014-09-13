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

-ifndef(DEBUG).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% RELEASE %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
-export([start_link/2, move_player/2, change_player_name/2]).
%% 状態関数
-export([move/2]).
%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).
-else.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% DEBUG %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-compile([debug_info, export_all]).
-endif.

-define(SERVER, ?MODULE).

-record(pstate, {
  wsservpid, % wsservのPid
  name, % プレイヤーの名前
  pos = {0, 0}, % キャラクターの位置
  ltime = 0, % 生存時間
  ehdlr % イベントハンドラ
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
  io:format("WSServPid: ~p Name: ~p~n", [WSServPid, Name]),
  gen_fsm:start_link(?MODULE, [WSServPid, Name], []).

%%--------------------------------------------------------------------
%% @doc
%% プレイヤーを動かす
%% @end
%%--------------------------------------------------------------------
move_player(PPid, XY = {_, _}) ->
  gen_fsm:send_event(PPid, {move, XY}).

%%--------------------------------------------------------------------
%% @doc
%% 名前変更を行う
%% @end
%%--------------------------------------------------------------------
change_player_name(PPid, Name) ->
  gen_fsm:send_all_state_event(PPid, {name, Name}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 初期化
%% 初期化後はidle状態にしてプレイヤーからのイベントを待つ
%% @end
%%--------------------------------------------------------------------
init([WSServPid, Name]) ->
  HandlerId = mmmario_game_event_handler:add_handler(),
  process_flag(trap_exit, true),
  link(WSServPid),
  {ok, move, #pstate{wsservpid = WSServPid, name = Name, ehdlr = HandlerId}}.

%%--------------------------------------------------------------------
%% @doc
%% 動作可能状態
%% moveイベントが来たらmove状態へ移動
%% @end
%%--------------------------------------------------------------------
move({move, {X, Y}}, S = #pstate{name = Name}) ->
  io:format("new posX: ~p posY: ~p~n", [X, Y]),
  mmmario_game_event_handler:notify({update_chara_pos, self(), Name, {X, Y}}),
  {next_state, move, S#pstate{pos = {X, Y}}}.

%%--------------------------------------------------------------------
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
%% @doc
%% 終了処理関数
%% ゲームイベントハンドラにプレイヤーが消えることを知らせておく
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _SName, #pstate{}) ->
  io:format("terminating player with: ~p~n", [Reason]),
  mmmario_game_event_handler:notify({delete_chara, self()}),
  ok.

code_change(_OldVsn, SName, S, _Extra) ->
  {ok, SName, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
