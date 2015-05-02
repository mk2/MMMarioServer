%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 8 2014 16:10
%%%-------------------------------------------------------------------
-module(mmmario_event_helper).
-author("lycaon").

%% API
-export([
  text_to_events/1,
  text_to_event/1
]).

-include("mmmario_game_type.hrl").

%%--------------------------------------------------------------------
%% @doc
%% XXXX|XXXX というような文字列をイベントリストに変換する
%% XXXXはイベント
%% @end
%%--------------------------------------------------------------------
text_to_events(RawText) ->
  lists:map(fun(Txt) -> text_to_event(Txt) end, string:tokens(RawText, "|")).

%%--------------------------------------------------------------------
%% @doc
%% 移動コマンドをイベントに変換
%% レクトを送信
%% ex: "MOV R1000,1000,32,32"
%% @end
%%--------------------------------------------------------------------
text_to_event("MOV " ++ RawText) ->
  Rect = text_to_rect(RawText),
  {move, Rect};

%%--------------------------------------------------------------------
%% @doc
%% 新規ブロック生成イベント
%% ex: "BLK R0,0,100,100" (0,0)に大きさ(100,100)のブロックを生成
%% @end
%%--------------------------------------------------------------------
text_to_event("BLK " ++ RawText) ->
  Rect = text_to_rect(RawText),
  {block, Rect};

%%--------------------------------------------------------------------
%% @doc
%% 死亡イベント
%% @end
%%--------------------------------------------------------------------
text_to_event("DIE" ++ _RawText) ->
  die;

%%--------------------------------------------------------------------
%% @doc
%% 名前登録、クライアントの名前を登録する
%% ex: "NAM 1203120"
%% @end
%%--------------------------------------------------------------------
text_to_event("NAM " ++ RawText) ->
  {name, RawText};

%%--------------------------------------------------------------------
%% @doc
%% 部屋一覧取得
%% ex: "RMS"
%% @end
%%--------------------------------------------------------------------
text_to_event("RMS" ++ _RawText) ->
  rooms;

%%--------------------------------------------------------------------
%% @doc
%% 新しい部屋生成
%% ex: "NRM"
%% @end
%%--------------------------------------------------------------------
text_to_event("NRM" ++ _RawText) ->
  new_room;

%%--------------------------------------------------------------------
%% @doc
%% どれにもマッチしないベント用
%% @end
%%--------------------------------------------------------------------
text_to_event(RawText) ->
  error_logger:error_msg("unknown command contained: ~p~n", [RawText]),
  undefined.
