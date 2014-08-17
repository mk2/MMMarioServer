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
-export([start_link/2, move_player/2]).
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

%% 開始メソッド
%% WSServPidはその名の通り、mmmario_wsservのpid
start_link(WSServPid, Name) ->
  io:format("WSServPid: ~p Name: ~p~n", [WSServPid, Name]),
  gen_fsm:start_link(?MODULE, [WSServPid, Name], []).

%% プレイヤーを動かす
move_player(PPid, XY = {_, _}) ->
  gen_fsm:send_event(PPid, {move, XY}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%% 初期化
%% 初期化後はidle状態にしてプレイヤーからのイベントを待つ
init([WSServPid, Name]) ->
  io:format("WSServPid: ~p~n", [WSServPid]),
  io:format("Name: ~p~n", [Name]),
  HandlerId = mmmario_event_handler:add_handler(mmmario_event_handler),
  {ok, move, #pstate{wsservpid = WSServPid, name = Name, ehdlr = HandlerId}}.

%% 動作可能状態
%% moveイベントが来たらmove状態へ移動
move({move, {X, Y}}, S = #pstate{}) ->
  io:format("new posX: ~p posY: ~p~n", [X, Y]),
  gen_event:notify(mmmario_event_handler, update_chara_pos),
  {next_state, move, S#pstate{pos = {X, Y}}}.

state_name(_Event, _From, S) ->
  Reply = ok,
  {reply, Reply, state_name, S}.

handle_event(_Event, SName, S) ->
  {next_state, SName, S}.

handle_sync_event(_Event, _From, SName, S) ->
  Reply = ok,
  {reply, Reply, SName, S}.

handle_info(_Info, SName, S) ->
  {next_state, SName, S}.

terminate(Reason, _SName, S = #pstate{ehdlr = HandlerId}) ->
  io:format("terminating with: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, SName, S, _Extra) ->
  {ok, SName, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================