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
-author("nijibox").

%% API
-export([start/2, stop/1]).

start(StartType, StartArgs) ->
  erlang:error(not_implemented).

stop(State) ->
  erlang:error(not_implemented).
