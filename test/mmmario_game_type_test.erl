%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 9 2014 14:02
%%%-------------------------------------------------------------------
-module(mmmario_game_type_test).
-author("lycaon").

-include_lib("eunit/include/eunit.hrl").
-include("../src/mmmario_game_type.hrl").

rect_convert_test() ->
  RectRawText = "R20,10,5,100",
  Rect = text_to_rect(RectRawText),
  RectText = rect_to_text(Rect),
  ?assertEqual(RectRawText, RectText).
