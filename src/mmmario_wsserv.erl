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

%% WebSocket周りの定数
-define(WEBSOCKET_PREFIX, "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: upgrade\r\nSec-Websocket-Accept: ").
-define(WEBSOCKET_APPEND_TO_KEY, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").

%% FINグループ
-define(FIN_ON, 2#1).
-define(FIN_OFF, 2#0).

%% RSVグループ
-define(RSV_ON, 2#1).
-define(RSV_OFF, 2#0).

%% OPCODE一覧
-define(OPCODE_CONTINUE, 16#0).
-define(OPCODE_TEXT, 16#1).
-define(OPCODE_BIN, 16#2).
-define(OPCODE_CLOSE, 16#8).
-define(OPCODE_PING, 16#9).
-define(OPCODE_PONG, 16#A).

%% MASKグループ
-define(MASK_ON, 2#1).
-define(MASK_OFF, 2#0).

%% Payload Length
-define(PAYLOAD_LENGTH_NORMAL, 10#125).
-define(PAYLOAD_LENGTH_EXTEND_16, 10#126).
-define(PAYLOAD_LENGTH_EXTEND_64, 10#127).

%% WebSocketのデータフレームを格納するレコード
-record(wsdataframe, {
  fin = ?FIN_OFF,
  rsv1 = ?RSV_OFF, rsv2 = ?RSV_OFF, rsv3 = ?RSV_OFF,
  opcode = ?OPCODE_TEXT,
  mask = ?MASK_OFF,
  pllen = 2#0,
  maskkey = 2#0,
  data,
  rawpacket}).

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
%% 受け取るパケットはすべてFIN=1だとする
client_loop(SPid, CSock) ->
  case gen_tcp:recv(CSock, 0) of

    {ok, Packet} ->
      WSDataFrame = decode_ws_dataframe(Packet),
      io:format("received dataframe: ~p~n", [WSDataFrame]),

      case WSDataFrame#wsdataframe.opcode of

        ?OPCODE_TEXT ->
          Data = WSDataFrame#wsdataframe.data,
          io:format("text data received: ~p~n", [Data]),
          gen_tcp:send(CSock, encode_ws_dataframe(Data, #{})), client_loop(SPid, CSock);

        ?OPCODE_CLOSE ->
          io:format("close request received~n"),
          gen_tcp:close(CSock);

        _Other -> erlang:error(unknown_opcode)
      end;

    {error, Reason} -> io:format("error Reason = ~p~n", [Reason]), exit(self(), Reason);

    _ -> erlang:error(unknown_msg)

  end.

%% websocketのデータフレームをデコード
decode_ws_dataframe(RawPacket) ->
  <<Fin:1, Rsv1:1, Rsv2:1, Rsv3:1, OpCode:4, Mask:1, PayloadLen:7, RemainPacket/binary>> = RawPacket,

  if PayloadLen =< ?PAYLOAD_LENGTH_NORMAL, Mask =:= ?MASK_ON ->
    io:format("PL NORMAL MASK ON~n"),
    {MaskKey, Data} = apply_mask_key(extract_mask_key(RemainPacket)),
    #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
      opcode = OpCode, maskkey = MaskKey, pllen = PayloadLen, data = Data, rawpacket = RawPacket};

    PayloadLen =< ?PAYLOAD_LENGTH_NORMAL, Mask =:= ?MASK_OFF ->
      io:format("PL NORMAL MASK OFF~n"),
      #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
        opcode = OpCode, maskkey = undefined, pllen = PayloadLen, data = RemainPacket, rawpacket = RawPacket};

    PayloadLen =:= ?PAYLOAD_LENGTH_EXTEND_16, Mask =:= ?MASK_ON ->
      io:format("PL EXTEND16 MASK ON~n"),
      {PayloadLen2, RemainPacket2} = extract_extend_payload_length_16(RemainPacket),
      {MaskKey, Data} = apply_mask_key(extract_mask_key(RemainPacket2)),
      #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
        opcode = OpCode, maskkey = MaskKey, pllen = PayloadLen2, data = Data, rawpacket = RawPacket};

    PayloadLen =:= ?PAYLOAD_LENGTH_EXTEND_16, Mask =:= ?MASK_OFF ->
      io:format("PL EXTEND16 MASK OFF~n"),
      {PayloadLen2, RemainPacket2} = extract_extend_payload_length_16(RemainPacket),
      #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
        opcode = OpCode, maskkey = undefined, pllen = PayloadLen2, data = RemainPacket2, rawpacket = RawPacket};

    PayloadLen =:= ?PAYLOAD_LENGTH_EXTEND_64, Mask =:= ?MASK_ON ->
      io:format("PL EXTEND64 MASK ON~n"),
      {PayloadLen2, RemainPacket2} = extract_extend_payload_length_64(RemainPacket),
      {MaskKey, Data} = apply_mask_key(extract_mask_key(RemainPacket2)),
      #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
        opcode = OpCode, maskkey = MaskKey, pllen = PayloadLen2, data = Data, rawpacket = RawPacket};

    PayloadLen =:= ?PAYLOAD_LENGTH_EXTEND_64, Mask =:= ?MASK_OFF ->
      io:format("PL EXTEND64 MASK OFF~n"),
      {PayloadLen2, RemainPacket2} = extract_extend_payload_length_64(RemainPacket),
      #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
        opcode = OpCode, maskkey = undefined, pllen = PayloadLen2, data = RemainPacket2, rawpacket = RawPacket};

    true -> erlang:error(unknown_payload_length)
  end.

%% マスクキーの抽出
extract_mask_key(RawPacket) ->
  <<MaskKey:4/binary-unit:8, RemainPacket/binary>> = RawPacket,
  {MaskKey, RemainPacket}.

%% 16ビット分のペイロード長を抽出
extract_extend_payload_length_16(RawPacket) ->
  <<Length:16/unsigned-integer, RemainPacket/binary>> = RawPacket,
  {Length, RemainPacket}.

%% 64ビット分のペイロード長を抽出
extract_extend_payload_length_64(RawPacket) ->
  <<Length:64/unsigned-integer, RemainPacket/binary>> = RawPacket,
  {Length, RemainPacket}.

%% マスクキーの適用
apply_mask_key({MaskKey, RawPacket}) ->
  apply_mask_key(RawPacket, MaskKey).
apply_mask_key(RawPacket, MaskKey) ->
  % RawPacketのサイズに合わせてMaskKeyのサイクルリストを作っておく
  RawPacketSize = byte_size(RawPacket),
  <<MK1:8, MK2:8, MK3:8, MK4:8>> = MaskKey,
  RawPacketSizeRemain = RawPacketSize rem 4,
  RemainMaskKeyList = case RawPacketSizeRemain of
                        1 -> [MK1];
                        2 -> [MK1, MK2];
                        3 -> [MK1, MK2, MK3];
                        _ -> []
                      end,
  MaskKeyList = lists:flatten(lists:duplicate(trunc(RawPacketSize / 4), [MK1, MK2, MK3, MK4])) ++ RemainMaskKeyList,
  RawPacketList = binary:bin_to_list(RawPacket),
  {MaskKey, binary:list_to_bin(lists:flatten([[Val bxor Mask] ||
    {Val, Mask} <- lists:zip(RawPacketList, MaskKeyList)]))}.
%%   {MaskKey, binary:list_to_bin(lists:flatten([[Val1 bxor MK1, Val2 bxor MK2, Val3 bxor MK3, Val4 bxor MK4] ||
%%     <<Val1:8, Val2:8, Val3:8, Val4:8>> <= RawPacket]))}.


%% データをwebsocketのデータフレームへエンコード
% とりあえずデータは1メッセージに収まるという想定
encode_ws_dataframe(Data, Opts) ->
  DataByteSize = byte_size(Data),
  FinRsvsOpCode = 2#10000000 bor 16#1,

  {{MaskKey, NewData}, MaskOnOffBits} = case maps:find(mask, Opts) of
                                          {ok, aMaskKey} ->
                                            {apply_mask_key(Data, aMaskKey), 2#10000000};
                                          _Other -> {{undefined, Data}, 2#00000000}
                                        end,

  if DataByteSize =< ?PAYLOAD_LENGTH_NORMAL, MaskKey =:= undefined ->
    MaskPayloadLength = MaskOnOffBits bor DataByteSize,
    <<FinRsvsOpCode, MaskPayloadLength, NewData/binary>>;

    DataByteSize =< ?PAYLOAD_LENGTH_NORMAL ->
      MaskPayloadLength = MaskOnOffBits bor DataByteSize,
      <<FinRsvsOpCode, MaskPayloadLength, MaskKey:16, NewData/binary>>;

    DataByteSize =:= ?PAYLOAD_LENGTH_EXTEND_16, MaskKey =:= undefined ->
      BaseMaskPayloadLength = MaskOnOffBits bor ?PAYLOAD_LENGTH_EXTEND_16,
      MaskPayloadLength = <<BaseMaskPayloadLength, DataByteSize:16/unsigned-integer>>,
      <<FinRsvsOpCode, MaskPayloadLength, NewData/binary>>;

    DataByteSize =:= ?PAYLOAD_LENGTH_EXTEND_16 ->
      BaseMaskPayloadLength = MaskOnOffBits bor ?PAYLOAD_LENGTH_EXTEND_16,
      MaskPayloadLength = <<BaseMaskPayloadLength, DataByteSize:16/unsigned-integer>>,
      <<FinRsvsOpCode, MaskPayloadLength, MaskKey:16, NewData/binary>>;

    DataByteSize =:= ?PAYLOAD_LENGTH_EXTEND_64, MaskKey =:= undefined ->
      BaseMaskPayloadLength = MaskOnOffBits bor ?PAYLOAD_LENGTH_EXTEND_64,
      MaskPayloadLength = <<BaseMaskPayloadLength, DataByteSize:64/unsigned-integer>>,
      <<FinRsvsOpCode, MaskPayloadLength, NewData/binary>>;

    DataByteSize =:= ?PAYLOAD_LENGTH_EXTEND_64 ->
      BaseMaskPayloadLength = MaskOnOffBits bor ?PAYLOAD_LENGTH_EXTEND_64,
      MaskPayloadLength = <<BaseMaskPayloadLength, DataByteSize:64/unsigned-integer>>,
      <<FinRsvsOpCode, MaskPayloadLength, MaskKey, NewData/binary>>;

    true -> erlang:error(unknown_payload_length)
  end.


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
  gen_tcp:send(Socket, encode_ws_dataframe(<<"test">>, #{})),
  receive
    All -> io:format("Recv: ~p~n", [All])
  end.

make_accept_header_value_test() ->
  SWKey = "E4WSEcseoWr4csPLS2QJHA==",
  AcceptHeaderValue = make_accept_header_value(SWKey),
  io:format("AcceptHeaderValue: ~p~n", [AcceptHeaderValue]),
  "7eQChgCtQMnVILefJAO6dK5JwPc=" = AcceptHeaderValue.

encode_ws_dataframe_test(Data) ->
  WSDataFrame = mmmario_wsserv:encode_ws_dataframe(Data, #{mask => <<4, 5, 1, 4>>}),
  io:format("WS Dataframe: ~p~n", [WSDataFrame]),
  #wsdataframe{data = Data} = mmmario_wsserv:decode_ws_dataframe(WSDataFrame).