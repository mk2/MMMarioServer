%%%-------------------------------------------------------------------
%%% @author nijibox
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 8 2014 11:23
%%%-------------------------------------------------------------------
-module(mmmario).
-behaviour(application).
-author("lycaon").

-ifndef(DEBUG).
%% API
-export([start/2, stop/1, new_player/2, move_player/2]).
-else.
-compile([debug_info, export_all]).
-endif.

start(_StartType, _StartArgs) ->
  mmmario_sup:start_link().

stop(State) ->
  erlang:error(not_implemented).

new_player(WSServPid, Name) ->
  io:format("WSServPid: ~p~n", [WSServPid]),
  mmmario_player_sup:start_player(WSServPid, Name).

move_player(PPid, Pos = {_, _}) ->
  mmmario_player:move_player(PPid, Pos).
