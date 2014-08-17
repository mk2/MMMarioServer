%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 8 2014 16:10
%%%-------------------------------------------------------------------
-module(mmmario_event).
-author("lycaon").

%% API
-export([text_to_event/1]).

text_to_event("M" ++ RawText) ->
  io:format("move event~n"),
  {move, {0, 0}};

text_to_event(RawText) ->
  io:format("unknown command contained: ~p~n", [RawText]).