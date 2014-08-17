%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 8 2014 14:34
%%%-------------------------------------------------------------------
-module(mmmario_player_test).
-author("lycaon").

-include_lib("eunit/include/eunit.hrl").

mmmario_player_sup_test() ->
  {ok, Pid} = mmmario_player_sup:start_link(),
  io:format("player supervisor pid: ~p~n", [Pid]),
  mmmario_player_sup:start_player(make_ref()).

