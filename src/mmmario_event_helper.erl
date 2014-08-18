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
-export([text_to_event/1, pos_list_to_binary/1]).
-compile([export_all]).

%% 移動コマンドをイベントに変換
%% ex: "M1000,1000"
text_to_event("m" ++ RawText) ->
  text_to_event("M" ++ RawText);
text_to_event("M" ++ RawText) ->
  [{X, _}, {Y, _} | _] = lists:map(fun(Txt) -> string:to_integer(Txt) end, string:tokens(RawText, ",")),
  io:format("X: ~p Y: ~p~n", [X, Y]),
  {move, {X, Y}};

text_to_event(RawText) ->
  io:format("unknown command contained: ~p~n", [RawText]).

%% 位置配列[{X, Y}, {X, Y} ... ]を文字列化する
%%
pos_list_to_binary(PosList) ->
  case catch lists:droplast(lists:flatten([[integer_to_list(X), ",", integer_to_list(Y), ","] || {X, Y} <- PosList])) of
    {'EXIT', _} -> failed;
    Str -> {ok, list_to_binary(Str)}
  end.
