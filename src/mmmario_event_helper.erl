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
-export([text_to_events/1, text_to_event/1, pos_list_to_binary/1]).
-compile([export_all]).

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
%% ex: "M1000,1000"
%% @end
%%--------------------------------------------------------------------
text_to_event("m" ++ RawText) ->
  text_to_event("M" ++ RawText);
text_to_event("M" ++ RawText) ->
  [{X, _}, {Y, _} | _] = lists:map(fun(Txt) -> string:to_integer(Txt) end, string:tokens(RawText, ",")),
  io:format("X: ~p Y: ~p~n", [X, Y]),
  {move, {X, Y}};

%%--------------------------------------------------------------------
%% @doc
%% 新規ブロック生成イベント
%% ex: "B0,0,100,100" (0,0)に大きさ(100,100)のブロックを生成
%% @end
%%--------------------------------------------------------------------
text_to_event("b" ++ RawText) ->
  text_to_event("B" ++ RawText);
text_to_event("B" ++ RawText) ->
  [{X, _}, {Y, _}, {W, _}, {H, _} | _] = lists:map(fun(Txt) -> string:to_integer(Txt) end, string:tokens(RawText, ",")),
  io:format("X: ~p Y: ~p W: ~p H: ~p~n", [X, Y, W, H]),
  {block, {X, Y, W, H}};


%%--------------------------------------------------------------------
%% @doc
%% 名前登録、クライアントの名前を登録する
%% ex: "N1203120"
%% @end
%%--------------------------------------------------------------------
text_to_event("n" ++ RawText) ->
  text_to_event("N" ++ RawText);
text_to_event("N" ++ RawText) ->
  {name, RawText};

text_to_event(RawText) ->
  error_logger:format("unknown command contained: ~p~n", [RawText]),
  undefined.

%%--------------------------------------------------------------------
%% @doc
%% 位置配列[{X, Y}, {X, Y} ... ]を文字列化する
%% @end
%%--------------------------------------------------------------------
pos_list_to_binary(PosList) ->
  case catch lists:droplast(
    lists:flatten(
      [[Name, ",", integer_to_list(X), ",", integer_to_list(Y), ","] || [Name, X, Y] <- PosList]
    )
  ) of
    {'EXIT', _} -> failed;
    Str -> {ok, list_to_binary(Str)}
  end.
