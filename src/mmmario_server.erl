%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 8 2014 0:49
%%%-------------------------------------------------------------------
-module(mmmario_server).
-behaviour(gen_server).
-author("lycaon").

-ifndef(DEBUGMODE).
%% API
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-else.
%% デバッグ用エクスポート
-compile([debug_info, export_all]).
-endif.

%% ゲームの状態を保持するレコード
-record(mario, {name, pos = {x, y}}).
-record(gameState, {marios = [], time = 0}).

%%--------------------------------------------------------------------
%% gen_server用コールバック
%%--------------------------------------------------------------------

%% gen_serverの開始
start() ->
  {ok, Pid} = gen_server:start_link(msrv, ?MODULE, [], [dbg, [trace, log]]).

%% 初期化コールバック
init(Args) ->
  {ok, #gameState{marios = [], time = 0}}.

%% 同期呼び出しコールバック
handle_call(Request, From, State) ->
  erlang:error(not_implemented).

%% 非同期呼び出しコールバック
handle_cast(Request, State) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  erlang:error(not_implemented).

terminate(Reason, State) ->
  erlang:error(not_implemented).

code_change(OldVsn, State, Extra) ->
  erlang:error(not_implemented).

%%--------------------------------------------------------------------
%% PRIVATE関数
%%--------------------------------------------------------------------

%% マリオの情報を更新
update_mario(Mario, GameState = #gameState{marios = Marios}) ->
  delete_mario(Mario, GameState),
  add_mario(Mario, GameState).

%% マリオの情報をゲーム状態に追加
add_mario(Mario = #mario{pos = {X, Y}}, GameState = #gameState{marios = Marios}) ->
  io:format("mario position : x = ~p, y = ~p~n", [X, Y]),
  GameState#gameState{marios = [Mario | Marios]}.

%% マリオの情報をゲーム状態に追加
delete_mario(Mario, GameState = #gameState{marios = Marios}) ->
  GameState#gameState{marios = lists:delete(Mario, Marios)}.

%% すべてのマリオの位置情報をリストとして取得
get_all_mario_pos(#gameState{marios = Marios}) ->
  [Pos || #mario{pos = Pos} <- Marios].
