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
-module(mmmario_wsserv).
-author("lycaon").

-ifndef(DEBUG).
%% API
-export([]).
-else.
%% デバッグ用エクスポート
-compile([debug_info, export_all]).
-endif.

-define(WEBSOCKET_PREFIX, "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: upgrade\r\nSec-Websocket-Accept: ").
-define(WEBSOCKET_APPEND_TO_KEY, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

%% 開始メソッド。MMMarioサーバーを開始後、待ちループに入る。
start(Port) ->
  % {_, SPid} = mmmario_server:start(),
  SPid = undefined,
  {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}, {packet, http}]),
  spawn_link(?MODULE, accepter_loop, [SPid, LSock]),
  {ok, SPid}.

%% TCPアクセプトが完了するまで待ち、その後ハンドシェイク処理を行った後クライアントループを起動。
%% ソケットの所有権の移譲をしないとポートが勝手に閉じる？
accepter_loop(SPid, LSock) ->
  io:format("waiting for connection.~n"),
  {ok, CSock} = gen_tcp:accept(LSock),
  case do_handshake(CSock, maps:new()) of
    {ok, _CSock} -> io:format("handshake passed.~n"),
      inet:setopts(CSock, [{packet, raw}]),
      CPid = spawn_link(?MODULE, client_loop, [SPid, CSock]),
      gen_tcp:controlling_process(CSock, CPid);
    _ -> accepter_loop(SPid, LSock)
  end.

%% ハンドシェイク処理
do_handshake(CSock, Headers) ->
  case gen_tcp:recv(CSock, 0) of
    {ok, {http_request, _Method, {abs_path, Path}, _Version}} ->
      do_handshake(CSock, Headers#{path => Path});
    {ok, {http_header, _, HttpField, _, Value}} ->
      HKey = {http_field, HttpField},
      do_handshake(CSock, maps:put(HKey, Value, Headers));
    {error, "\r\n"} -> do_handshake(CSock, Headers);
    {error, "\n"} -> do_handshake(CSock, Headers);
    {ok, http_eoh} -> % ヘッダが終了したらハンドシェイク処理に入る
      verify_handshake(CSock, Headers);
    _Others ->
      io:format("Unknown data: ~p~n", [_Others]),
      io:format("Headers: ~p~n", [Headers]),
      gen_tcp:close(CSock),
      exit(normal)
  end,
  {ok, CSock}.

%% ヘッダー情報をまるっと受け取ったらここでヘッダーの内容をチェックし、大丈夫ならsend_handshakeを呼び出して
%% openingハンドシェイクを送信する
verify_handshake(CSock, Headers) ->
  io:format("Headers: ~p~n", [Headers]),
  catch true = string:equal("websocket", string:to_lower(maps:get({http_field, 'Upgrade'}))),
  catch true = string:equal("upgrade", string:to_lower(maps:get({http_field, 'Connection'}))),
  catch true = string:equal("13", string:to_lower(maps:get({http_field, "Sec-Websocket-Version"}))),
  catch {ok, _} = maps:find({http_field, "Sec-Websocket-Key"}, Headers),
  send_handshake(CSock, Headers),
  {ok, CSock}.

%% openingハンドシェイクを送信する
send_handshake(CSock, Headers) ->
  SWKey = maps:get({http_field, "Sec-Websocket-Key"}, Headers),
  AcceptHeaderValue = make_accept_header_value(SWKey),
  AcceptHeader = ?WEBSOCKET_PREFIX ++ AcceptHeaderValue ++ "\r\n\r\n",
  gen_tcp:send(CSock, AcceptHeader),
  {ok, CSock}.

%% websocketのアクセプトヘッダの値を作る
make_accept_header_value(SWKey) ->
  % WEBSOCKET_APPEND_TO_KEYを後ろに連結
  NewKey = SWKey ++ ?WEBSOCKET_APPEND_TO_KEY,
  % sha1でダイジェストを得る
  Digest = crypto:hash(sha, NewKey),
  % base64で符号化
  base64:encode_to_string(Digest).

%% クライアントループ
client_loop(SPid, CSock) ->
  case gen_tcp:recv(CSock, 0) of
    {ok, Packet} ->
      io:format("packet is ~p~n", [Packet]),
      {Fin, OpCode, Mask, PayloadLen, Data} = decode_ws_dataframe(Packet),
      io:format("Data: ~p~n", [Data]),
      io:format("PayloadLen: ~p~n", [PayloadLen]),
      gen_tcp:send(CSock, encode_ws_dataframe(<<"t">>, {})), client_loop(SPid, CSock);
    {error, Reason} -> io:format("error Reason = ~p~n", [Reason]), exit(self(), Reason);
    _ -> erlang:error(unknown_msg)
  end.

%% websocketのデータフレームをデコード
decode_ws_dataframe(DataFrame) ->
  <<Fin:1, _Rsvs:3, OpCode:4, Mask:1, PayloadLen:7, RemainDataFrame/binary>> = DataFrame,
  {Fin, OpCode, Mask, PayloadLen, RemainDataFrame}.

%% データをwebsocketのデータフレームへエンコード
encode_ws_dataframe(Data, Opts) ->
  % とりあえずデータは1メッセージに収まるという想定
  FinRsvsOpCode = 2#10000000 bor 16#1,
  DataByteSize = byte_size(Data),
  MaskPayloadLength = 2#00000000 bor DataByteSize,
  <<FinRsvsOpCode, MaskPayloadLength, Data/binary>>.

%%%-------------------------------------------------------------------
%%% テスト関数
%%%-------------------------------------------------------------------
mmmario_wsserv_test() ->
  PortNum = 8080,
  {ok, SPid} = start(PortNum),
  {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, PortNum, [binary, {active, true}, {packet, http}]),
  InitialSampleRequestHeader = ""
    ++ "GET /resource HTTP/1.1\r\n"
    ++ "Host: localhost\r\n"
    ++ "Upgrade: websocket\r\n"
    ++ "Connection: upgrade\r\n"
    ++ "Sec-Websocket-Version: 13\r\n"
    ++ "Sec-Websocket-Key: E4WSEcseoWr4csPLS2QJHA==\r\n"
    ++ "\r\n",
  gen_tcp:send(Socket, InitialSampleRequestHeader),
  mmmario_wsserv_test_loop(Socket).

mmmario_wsserv_test_loop(Socket) ->
  receive
    {tcp, _Port, Msg} -> io:format("msg = ~p~n", [Msg]), mmmario_wsserv_test_loop(Socket);
    {http, _Port, {http_response, _Version, Status, Msg}} ->
      io:format("response status: ~p   msg: ~p~n", [Status, Msg]), mmmario_wsserv_test_loop(Socket);
    {http, _Port, {http_header, _Version, Header, _, Value}} ->
      io:format("header header: ~p   value: ~p~n", [Header, Value]), mmmario_wsserv_test_loop(Socket);
    {http, _Port, http_eoh} ->
      io:format("All headers recieved.~n"),
      inet:setopts(Socket, [{packet, raw}]),
      mmmario_wsserv_test_msg_loop(Socket);
    What -> io:format("what = ~p~n", [What]), erlang:error(What)
  end.

mmmario_wsserv_test_msg_loop(Socket) ->
  gen_tcp:send(Socket, encode_ws_dataframe(<<"test">>, {})),
  receive
    All -> io:format("Recv: ~p~n", [All])
  end.

make_accept_header_value_test() ->
  SWKey = "E4WSEcseoWr4csPLS2QJHA==",
  AcceptHeaderValue = make_accept_header_value(SWKey),
  io:format("AcceptHeaderValue: ~p~n", [AcceptHeaderValue]),
  "7eQChgCtQMnVILefJAO6dK5JwPc=" = AcceptHeaderValue.

encode_ws_dataframe_test(Data) ->
  WSDataFrame = mmmario_wsserv:encode_ws_dataframe(Data, {}),
  io:format("WS Dataframe: ~p~n", [WSDataFrame]),
  {1, 1, 0, 4, <<"test">>} = mmmario_wsserv:decode_ws_dataframe(WSDataFrame).