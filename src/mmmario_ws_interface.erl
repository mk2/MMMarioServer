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
  % {_, SPid} = mmmario_server:start(),
  SPid = undefined,
  {ok, LSock} = gen_tcp:listen(Port, [list, {active, false}]),
  spawn_link(?MODULE, accepter_loop, [SPid, LSock]),
  {ok, SPid}.

%% TCPアクセプトが完了するまで待ち、その後ハンドシェイク処理を行った後クライアントループを起動。
accepter_loop(SPid, LSock) ->
  {ok, CSock} = gen_tcp:accept(LSock),
  io:format("waiting for connection.~n"),
  case do_handshake(CSock) of
    {ok, _CSock} -> io:format("handshake passed.~n"), spawn_link(?MODULE, client_loop, [SPid, CSock]);
    _ -> accepter_loop(SPid, LSock)
  end.

%% ハンドシェイク処理
do_handshake(CSock) ->
  {ok, CSock}.

%% クライアントループ
client_loop(SPid, CSock) ->
  case gen_tcp:recv(CSock, 0) of
    {ok, Packet} -> io:format("packet is ~p~n", [Packet]), gen_tcp:send(CSock, Packet), client_loop(SPid, CSock);
    {error, Reason} -> io:format("error Reason = ~p~n", [Reason]), exit(self(), Reason);
    _ -> erlang:error(unknown_msg)
  end.

%%%-------------------------------------------------------------------
%%% テスト関数
%%%-------------------------------------------------------------------
mmmario_ws_test() ->
  PortNum = 8080,
  {ok, SPid} = start(PortNum),
  {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, PortNum, [list, {active, true}]),
  gen_tcp:send(Socket, "test packet"),
  receive
    {tcp, _Port, Msg} -> io:format("msg = ~p~n", [Msg]);
    What -> io:format("what = ~p~n", [What]), erlang:error(What)
  end.
