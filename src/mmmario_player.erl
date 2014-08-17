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
  ltime = 0 % 生存時間
}).

%%%===================================================================
%%% API
%%%===================================================================

%% 開始メソッド
%% WSServPidはその名の通り、mmmario_wsservのpid
start_link(WSServPid, Name) ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [WSServPid, Name], []).

%% プレイヤーを動かす
move_player(PPid, Dxy = {_, _}) ->
  gen_fsm:send_event(PPid, {move, Dxy}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([WSServPid, Name]) ->
  io:format("WSServPid: ~p~n", [WSServPid]),
  io:format("Name: ~p~n", [Name]),
  {ok, move, #pstate{wsservpid = WSServPid, name = Name}}.

%% 動作可能状態
%% moveイベントが来たらmove状態へ移動
move({move, {X, Y}}, S = #pstate{}) ->
  {next_state, move, S#pstate{pos = {X, Y}}}.

state_name(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
