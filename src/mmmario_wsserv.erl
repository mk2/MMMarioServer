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
-behaviour(gen_server).
-author("lycaon").

-ifndef(DEBUG).
%% APIs
%% 公開API
-export([start_link/1, send/2]).
%% gen_serverコールバックAPI
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
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
-define(MAX_UNSIGNED_INTEGER_16, 65532).
-define(MAX_UNSIGNED_INTEGER_64, 9223372036854775807).

%% handle_infoで楽にTCPパケットを受け取るためのマクロ
-define(SOCK(Msg), {tcp, _Port, Msg}).

%% WebSocketのデータフレームを格納するレコード
-record(wsdataframe, {
  fin = ?FIN_OFF,
  rsv1 = ?RSV_OFF, rsv2 = ?RSV_OFF, rsv3 = ?RSV_OFF,
  opcode = ?OPCODE_TEXT,
  mask = ?MASK_OFF,
  pllen = 2#0,
  maskkey = 2#0,
  data,
  msg
}).

%% wsserveの状態
-record(wsservstate, {
  lsock,
  csock,
  ppid, % Player FSMのPid
  wsdataframe = #wsdataframe{}
}).

%%%-------------------------------------------------------------------
%%% 公開API
%%%-------------------------------------------------------------------

%% 開始メソッド。supervisorから起動される。ListenSocketはsupervisorから受け取る
start_link(LSock) ->
  gen_server:start_link(?MODULE, LSock, []).

%% データ送信メソッド
send(Pid, Data) ->
  gen_server:cast(Pid, {data, Data}).

%%%-------------------------------------------------------------------
%%% gen_server コールバック
%%%-------------------------------------------------------------------

%% gen_serverコールバック
%% accept処理をここでやらずにcastしてるのは、accept処理中はポーリング状態になるため。{ok, _}をすぐに返さないと怒られる希ガス
init(LSock) ->
  gen_server:cast(self(), accept),
  {ok, #wsservstate{lsock = LSock}}.

%% 同期呼び出し
%% 今のところ使う予定はないが、内部的にwsservを呼ぶときに使ったほうがよい？
handle_call(_Request, _From, State) ->
  {ok, State}.

%% 正常なメッセージを裁く関数
%% ここからメッセージハンドラに流していく
handle_info(?SOCK(Msg), S = #wsservstate{}) ->
  WSDataFrame = decode_ws_dataframe(Msg),
  io:format("received dataframe: ~p~n", [WSDataFrame]),
  gen_server:cast(self(), wsrequest),
  {noreply, S#wsservstate{wsdataframe = WSDataFrame}};

%% エラー処理
%%
handle_info({tcp_closed, _CSock}, S = #wsservstate{}) ->
  {stop, normal, S};
handle_info({tcp_error, _CSock, _}, S = #wsservstate{}) ->
  {stop, normal, S};

%% 未知のメッセージ処理
%% 無視して別のメッセージを待つ
handle_info(Msg, S) ->
  io:format("unexpected msg: ~p~n", [Msg]),
  {noreply, S}.

%% TCPアクセプトを処理する関数
%% TCPアクセプトが完了するまで待ち、その後ハンドシェイク処理を行った後クライアントループを起動。
handle_cast(accept, S = #wsservstate{lsock = LSock}) ->
  io:format("waiting for connection.~n"),
  {ok, CSock} = gen_tcp:accept(LSock),
  % 別のwsservを１個起動しておく
  mmmario_wsserv_sup:start_wsserv(),
  case do_handshake(CSock, maps:new()) of
    {ok, _} -> io:format("handshake passed.~n"),
      inet:setopts(CSock, [{packet, raw}, {active, once}]), % ハンドシェイクが終わったらアクティブモードで起動
      {ok, PPid} = mmmario:join_player(self(), "test"), % キャラクターのFSMを起動しておく
      {noreply, S#wsservstate{csock = CSock, ppid = PPid}};
    {stop, Reason, _} -> {stop, Reason, S};
    _ -> {stop, "failed handshake with unknown reason", S}
  end;

%% テキストメッセージを処理
%%
handle_cast(
    wsrequest,
    S = #wsservstate{wsdataframe = WSDataFrame, csock = CSock}
) when WSDataFrame#wsdataframe.opcode =:= ?OPCODE_TEXT ->
  Data = WSDataFrame#wsdataframe.data,
  io:format("text data received: ~p~n", [Data]),
  SendWSDataFrame = encode_ws_dataframe(Data, #{}),
  io:format("send dataframe: ~p~n", [SendWSDataFrame]),
  gen_tcp:send(CSock, SendWSDataFrame),
  inet:setopts(CSock, [{active, once}]),
  {noreply, S};

%% クローズメッセージを処理
%% Socketはterminateの方で閉じる？
handle_cast(
    wsrequest,
    S = #wsservstate{wsdataframe = WSDataFrame, csock = CSock}
) when WSDataFrame#wsdataframe.opcode =:= ?OPCODE_CLOSE ->
  io:format("close request received~n"),
  gen_tcp:close(CSock),
  {stop, close_request, S};

%% PINGメッセージを処理
%% PONGを返す
handle_cast(
    wsrequest,
    S = #wsservstate{wsdataframe = WSDataFrame, csock = CSock}
) when WSDataFrame#wsdataframe.opcode =:= ?OPCODE_PING ->
  io:format("ping request received~n"),
  gen_tcp:send(CSock, encode_ws_dataframe("", #{opcode => ?OPCODE_PONG})),
  inet:setopts(CSock, [{active, once}]),
  {noreply, S};

%% 任意のデータ送信。send/2経由で使う
%% 今のところOpCodeはTEXTになる
handle_cast(
    {data, Data},
    S = #wsservstate{csock = CSock}
) ->
  io:format("data will be sent: ~p~n", [Data]),
  WSDataFrame = encode_ws_dataframe(Data, {}),
  gen_tcp:send(CSock, WSDataFrame),
  {ok, S#wsservstate{wsdataframe = WSDataFrame}}.

%% gen_serverコールバック
%% クライアントブラウザが閉じて強制的に終了する場合はここが呼ばれる
terminate(Reason, S = #wsservstate{csock = CSock}) ->
  io:format("terminating: ~p~n", [Reason]),
  gen_tcp:close(CSock),
  {stop, Reason, S}.

%% gen_serverコールバック
%%
code_change(OldVsn, State, Extra) ->
  erlang:error(not_implemented).

%%%-------------------------------------------------------------------
%%% 内部的に使う関数
%%%-------------------------------------------------------------------

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
      verify_handshake(CSock, Headers),
      {ok, [{headers, Headers}]};
    Others ->
      io:format("Unknown msg: ~p~n", [Others]),
      io:format("Headers: ~p~n", [Headers]),
      {stop, unknown_header, [{headers, Headers}, {msg, Others}]}
  end.

%% ヘッダー情報をまるっと受け取ったらここでヘッダーの内容をチェックし、大丈夫ならsend_handshakeを呼び出して
%% openingハンドシェイクを送信する
verify_handshake(CSock, Headers) ->
  io:format("Headers: ~p~n", [Headers]),
  catch true = string:equal("websocket", string:to_lower(maps:get({http_field, 'Upgrade'}))),
  catch true = string:equal("upgrade", string:to_lower(maps:get({http_field, 'Connection'}))),
  catch true = string:equal("13", string:to_lower(maps:get({http_field, "Sec-Websocket-Version"}))),
  catch {ok, _} = maps:find({http_field, "Sec-Websocket-Key"}, Headers),
  send_handshake(CSock, Headers).

%% openingハンドシェイクを送信する
send_handshake(CSock, Headers) ->
  SWKey = maps:get({http_field, "Sec-Websocket-Key"}, Headers),
  AcceptHeaderValue = make_accept_header_value(SWKey),
  AcceptHeader = ?WEBSOCKET_PREFIX ++ AcceptHeaderValue ++ "\r\n\r\n",
  gen_tcp:send(CSock, AcceptHeader).

%% websocketのアクセプトヘッダの値を作る
make_accept_header_value(SWKey) ->
  % WEBSOCKET_APPEND_TO_KEYを後ろに連結
  NewKey = SWKey ++ ?WEBSOCKET_APPEND_TO_KEY,
  % sha1でダイジェストを得る
  Digest = crypto:hash(sha, NewKey),
  % base64で符号化
  base64:encode_to_string(Digest).

%% websocketのデータフレームをデコード
decode_ws_dataframe(RawMsg) ->
  <<Fin:1, Rsv1:1, Rsv2:1, Rsv3:1, OpCode:4, Mask:1, PayloadLen:7, RemainMsg/binary>> = RawMsg,

  if PayloadLen =< ?PAYLOAD_LENGTH_NORMAL, Mask =:= ?MASK_ON ->
    io:format("PL NORMAL MASK ON~n"),
    {MaskKey, Data} = apply_mask_key(extract_mask_key(RemainMsg)),
    #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
      opcode = OpCode, mask = ?MASK_ON, maskkey = MaskKey, pllen = PayloadLen, data = Data, msg = RawMsg};

    PayloadLen =< ?PAYLOAD_LENGTH_NORMAL, Mask =:= ?MASK_OFF ->
      io:format("PL NORMAL MASK OFF~n"),
      #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
        opcode = OpCode, mask = ?MASK_OFF, maskkey = undefined,
        pllen = PayloadLen, data = RemainMsg, msg = RawMsg};

    PayloadLen =:= ?PAYLOAD_LENGTH_EXTEND_16, Mask =:= ?MASK_ON ->
      io:format("PL EXTEND16 MASK ON~n"),
      {PayloadLen2, RemainMsg2} = extract_extend_payload_length_16(RemainMsg),
      {MaskKey, Data} = apply_mask_key(extract_mask_key(RemainMsg2)),
      #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
        opcode = OpCode, mask = ?MASK_ON, maskkey = MaskKey,
        pllen = PayloadLen2, data = Data, msg = RawMsg};

    PayloadLen =:= ?PAYLOAD_LENGTH_EXTEND_16, Mask =:= ?MASK_OFF ->
      io:format("PL EXTEND16 MASK OFF~n"),
      {PayloadLen2, RemainMsg2} = extract_extend_payload_length_16(RemainMsg),
      #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
        opcode = OpCode, mask = ?MASK_OFF, maskkey = undefined,
        pllen = PayloadLen2, data = RemainMsg2, msg = RawMsg};

    PayloadLen =:= ?PAYLOAD_LENGTH_EXTEND_64, Mask =:= ?MASK_ON ->
      io:format("PL EXTEND64 MASK ON~n"),
      {PayloadLen2, RemainMsg2} = extract_extend_payload_length_64(RemainMsg),
      {MaskKey, Data} = apply_mask_key(extract_mask_key(RemainMsg2)),
      #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
        opcode = OpCode, mask = ?MASK_ON, maskkey = MaskKey,
        pllen = PayloadLen2, data = Data, msg = RawMsg};

    PayloadLen =:= ?PAYLOAD_LENGTH_EXTEND_64, Mask =:= ?MASK_OFF ->
      io:format("PL EXTEND64 MASK OFF~n"),
      {PayloadLen2, RemainMsg2} = extract_extend_payload_length_64(RemainMsg),
      #wsdataframe{fin = Fin, rsv1 = Rsv1, rsv2 = Rsv2, rsv3 = Rsv3,
        opcode = OpCode, mask = ?MASK_OFF, maskkey = undefined,
        pllen = PayloadLen2, data = RemainMsg2, msg = RawMsg};

    true -> erlang:error(unknown_payload_length)
  end.


%% データをwebsocketのデータフレームへエンコード
% とりあえずデータは1メッセージに収まるという想定
encode_ws_dataframe(Data, Opts) ->
  DataByteSize = byte_size(Data),
  FinRsvsOpCode = case maps:find(opcode, Opts) of
                    {ok, ?OPCODE_PONG} -> 2#10000000 bor ?OPCODE_PONG;
                    _Other -> 2#10000000 bor ?OPCODE_TEXT
                  end,

  {{MaskKey, NewData}, MaskOnOffBits} = case maps:find(mask, Opts) of
                                          {ok, aMaskKey} ->
                                            {apply_mask_key(Data, aMaskKey), 2#10000000};
                                          _ -> {{undefined, Data}, 2#00000000}
                                        end,

  if DataByteSize =< ?PAYLOAD_LENGTH_NORMAL, MaskKey =:= undefined ->
    io:format("PL NORMAL MASK OFF~n"),
    MaskPayloadLength = MaskOnOffBits bor DataByteSize,
    <<FinRsvsOpCode, MaskPayloadLength, NewData/binary>>;

    DataByteSize =< ?PAYLOAD_LENGTH_NORMAL ->
      io:format("PL NORMAL MASK ON~n"),
      MaskPayloadLength = MaskOnOffBits bor DataByteSize,
      <<FinRsvsOpCode, MaskPayloadLength, MaskKey:16, NewData/binary>>;

    ?PAYLOAD_LENGTH_NORMAL < DataByteSize, DataByteSize =< ?MAX_UNSIGNED_INTEGER_16, MaskKey =:= undefined ->
      io:format("PL EXTEND16 MASK OFF~n"),
      BaseMaskPayloadLength = MaskOnOffBits bor ?PAYLOAD_LENGTH_EXTEND_16,
      <<FinRsvsOpCode, BaseMaskPayloadLength, DataByteSize:2/unsigned-integer-unit:8, NewData/binary>>;

    ?PAYLOAD_LENGTH_NORMAL < DataByteSize, DataByteSize =< ?MAX_UNSIGNED_INTEGER_16 ->
      io:format("PL EXTEND16 MASK ON~n"),
      BaseMaskPayloadLength = MaskOnOffBits bor ?PAYLOAD_LENGTH_EXTEND_16,
      <<FinRsvsOpCode, BaseMaskPayloadLength, DataByteSize:2/unsigned-integer-unit:8, MaskKey:16, NewData/binary>>;

    ?MAX_UNSIGNED_INTEGER_16 < DataByteSize, DataByteSize =< ?MAX_UNSIGNED_INTEGER_64, MaskKey =:= undefined ->
      io:format("PL EXTEND64 MASK OFF~n"),
      BaseMaskPayloadLength = MaskOnOffBits bor ?PAYLOAD_LENGTH_EXTEND_64,
      <<FinRsvsOpCode, BaseMaskPayloadLength, DataByteSize:8/unsigned-integer-unit:8, NewData/binary>>;

    ?MAX_UNSIGNED_INTEGER_16 < DataByteSize, DataByteSize =< ?MAX_UNSIGNED_INTEGER_64 ->
      io:format("PL EXTEND64 MASK ON~n"),
      BaseMaskPayloadLength = MaskOnOffBits bor ?PAYLOAD_LENGTH_EXTEND_64,
      <<FinRsvsOpCode, BaseMaskPayloadLength, DataByteSize:8/unsigned-integer-unit:8, MaskKey:16, NewData/binary>>;

    true -> erlang:error(unknown_payload_length)
  end.

%%%-------------------------------------------------------------------
%%% WebSocket用ユーティリティ関数
%%%-------------------------------------------------------------------

%% マスクキーの抽出
extract_mask_key(RawMsg) ->
  <<MaskKey:4/binary-unit:8, RemainMsg/binary>> = RawMsg,
  {MaskKey, RemainMsg}.

%% 16ビット分のペイロード長を抽出
extract_extend_payload_length_16(RawMsg) ->
  <<Length:16/unsigned-integer, RemainMsg/binary>> = RawMsg,
  {Length, RemainMsg}.

%% 64ビット分のペイロード長を抽出
extract_extend_payload_length_64(RawMsg) ->
  <<Length:64/unsigned-integer, RemainMsg/binary>> = RawMsg,
  {Length, RemainMsg}.

%% マスクキーの適用
apply_mask_key({MaskKey, RawMsg}) ->
  apply_mask_key(RawMsg, MaskKey).
apply_mask_key(RawMsg, MaskKey) ->
  % RawMsgのサイズに合わせてMaskKeyのサイクルリストを作っておく
  RawMsgSize = byte_size(RawMsg),
  <<MK1:8, MK2:8, MK3:8, MK4:8>> = MaskKey,
  RawMsgSizeRemain = RawMsgSize rem 4,
  RemainMaskKeyList = case RawMsgSizeRemain of
                        1 -> [MK1];
                        2 -> [MK1, MK2];
                        3 -> [MK1, MK2, MK3];
                        _ -> []
                      end,
  MaskKeyList = lists:flatten(lists:duplicate(trunc(RawMsgSize / 4), [MK1, MK2, MK3, MK4])) ++ RemainMaskKeyList,
  RawMsgList = binary:bin_to_list(RawMsg),
  {MaskKey, binary:list_to_bin(lists:flatten([[Val bxor Mask] ||
    {Val, Mask} <- lists:zip(RawMsgList, MaskKeyList)]))}.
%%   {MaskKey, binary:list_to_bin(lists:flatten([[Val1 bxor MK1, Val2 bxor MK2, Val3 bxor MK3, Val4 bxor MK4] ||
%%     <<Val1:8, Val2:8, Val3:8, Val4:8>> <= RawMsg]))}.


%%%-------------------------------------------------------------------
%%% テスト関数
%%%-------------------------------------------------------------------
make_accept_header_value_test() ->
  SWKey = "E4WSEcseoWr4csPLS2QJHA==",
  AcceptHeaderValue = make_accept_header_value(SWKey),
  io:format("AcceptHeaderValue: ~p~n", [AcceptHeaderValue]),
  "7eQChgCtQMnVILefJAO6dK5JwPc=" = AcceptHeaderValue.

encode_ws_dataframe_test(Data) ->
  WSDataFrame = mmmario_wsserv:encode_ws_dataframe(Data, #{mask => <<4, 5, 1, 4>>}),
  io:format("WS Dataframe: ~p~n", [WSDataFrame]),
  #wsdataframe{data = Data} = mmmario_wsserv:decode_ws_dataframe(WSDataFrame).

