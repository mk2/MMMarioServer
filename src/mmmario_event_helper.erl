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
-export([text_to_event/1]).

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