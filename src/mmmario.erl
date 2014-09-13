%%%-------------------------------------------------------------------
%%% @author lycaon
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
-export([start/2, stop/1, new_player/2, exit_player/1, move_player/2, change_player_name/2]).
-else.
-compile([debug_info, export_all]).
-endif.

start(_StartType, _StartArgs) ->
  mmmario_sup:start_link().

stop(_State) ->
  ok.

new_player(WSServPid, Name) ->
  mmmario_player_sup:start_player(WSServPid, Name).

exit_player(PPid) ->
  mmmario_player_sup:exit_player(PPid).

move_player(PPid, Pos = {_, _}) ->
  mmmario_player:move_player(PPid, Pos).

change_player_name(PPid, Name) ->
  mmmario_player:change_player_name(PPid, Name).
