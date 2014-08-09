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

%% マリオレコード
-record(mario, {name, pos = {0, 0}}).
%% ゲームの状態を保持するレコード
%% mariosは{pid(), mario}のタプルリスト
%% dispsは{pid(), displayInfo}のタプルリスト
-record(gameState, {marios = [], time = 0, disps = []}).
%% 表示するデバイスの情報を保持するレコード
-record(displayInfo, {rect = {0, 0, 600, 400}}).

%%--------------------------------------------------------------------
%% gen_server用コールバック
%%--------------------------------------------------------------------

%% gen_serverの開始
start() ->
  gen_server:start_link({global, mmmsrv}, ?MODULE, [], [dbg, [trace, log]]).

%% 初期化コールバック
init(Args) ->
  {ok, #gameState{}}.

%% 同期呼び出しコールバック
handle_call(Request, From, State) ->
  {{success, Reply}, NewState} = request_handler(Request, From, State),
  {reply, Reply, NewState}.

%% 非同期呼び出しコールバック
handle_cast(Request, State) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  erlang:error(not_implemented).

terminate(Reason, State) ->
  io:format("terminate called, last server state = ~p~n", [State]),
  case Reason of
    normal -> ok;
    shutdonw -> ok;
    {shutdown, something} -> ok;
    something -> ok;
    _ -> ok
  end.

code_change(OldVsn, State, Extra) ->
  erlang:error(not_implemented).

%%--------------------------------------------------------------------
%% PRIVATE関数
%%--------------------------------------------------------------------

%% リクエストを処理
%% リクエストの内容に応じて状態を更新し、それを返す。
request_handler(Request, {Pid, Tag}, State) ->
  case Request of

    {join, DisplayInfo} -> % 参加する場合
      NewState = join(Pid, DisplayInfo, State),
      AllMarioPos = get_all_mario_pos(NewState#gameState.marios),
      {{success, AllMarioPos}, NewState};

    {move, Mario} -> % マリオを動かす場合
      NewState = move(Pid, Mario, State),
      AllMarioPos = get_all_mario_pos(NewState#gameState.marios),
      {{success, AllMarioPos}, NewState};

    {change_display, DisplayInfo} -> % 表示を変更する場合
      NewState = change_display(Pid, DisplayInfo, State),
      AllMarioPos = get_all_mario_pos(NewState#gameState.marios),
      {{success, AllMarioPos}, NewState};

    {retire} -> % ゲームを辞める場合
      {{success, "Thank you for playing."}, retire(Pid, State)};

    _ -> % 何にもマッチしないとき
      erlang:eror(no_matching_command)
  end.

%%
%% 実際の処理を行う関数
%%

%% ゲームに参加する時呼ぶ
join(Key, DisplayInfo, GameState = #gameState{marios = Marios, disps = Disps}) ->
  Exist = lists:keyfind(Key, 1, Disps),
  if Exist =:= false ->
    io:format("pid = ~p~n", [Key]),
    GameState#gameState{marios = [{Key, #mario{name = uid()}} | Marios], disps = [{Key, DisplayInfo} | Disps]};
    true -> erlang:error(already_joined)
  end.


%% マリオを動かす
move(Key, Mario, GameState = #gameState{marios = Marios}) ->
  GameState#gameState{marios = lists:keystore(Key, 1, Marios, {Key, Mario})}.

%% 表示情報の更新
change_display(Key, NewDisplayInfo, GameState = #gameState{disps = Disps}) ->
  GameState#gameState{disps = lists:keystore(Key, 1, Disps, {Key, NewDisplayInfo})}.

%% ゲームを辞めるとき呼ぶ
retire(Key, GameState = #gameState{marios = Marios, disps = Disps}) ->
  GameState#gameState{marios = lists:keydelete(Key, 1, Marios), disps = lists:keydelete(Key, 1, Disps)}.

%%
%% マリオの操作関数
%%

%% すべてのマリオの位置情報をリストとして取得
get_all_mario_pos(Marios) ->
  [Mario#mario.pos || {_Key, Mario} <- Marios].

%%
%% ユーティリティ関数
%%
uid() ->
  {node(), now()}.