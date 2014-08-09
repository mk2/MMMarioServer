%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%% MMMarioサーバーのwebsocketインターフェース
%%%
%%%
%%% @end
%%% Created : 10. 8 2014 0:16
%%%-------------------------------------------------------------------
-module(mmmario_ws_interface).
-author("lycaon").

-ifndef(DEBUG).
%% API
-export([]).
-else.
%% デバッグ用エクスポート
-compile([debug_info, export_all]).
-endif.

%% 開始メソッド。MMMarioサーバーを開始後、待ちループに入る。
start(Port) ->
  {ok, SPid} = mmmario_server:start(),
  {ok, LSock} = gen_tcp:listen(Port, [list, {packet, http}, {active, false}]),
  spawn_monitor(?MODULE, accepter_loop, [SPid, LSock]).

%% TCPアクセプトが完了するまで待ち、その後ハンドシェイク処理を行った後クライアントループを起動。
accepter_loop(SPid, LSock) ->
  {ok, CSock} = gen_tcp:accept(LSock),
  case do_handshake(CSock) of
    {ok, _CSock} -> spawn_monitor(?MODULE, cliend_loop, [SPid, CSock]);
    _ -> accepter_loop(SPid, LSock)
  end.

%% ハンドシェイク処理
do_handshake(CSock) ->
  {ok, CSock}.

%% クライアントループ
cliend_loop(SPid, CSock) ->
  io:format("client loop start~n"),
  {ok, SPid, CSock}.
