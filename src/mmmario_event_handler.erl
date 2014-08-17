%%%-------------------------------------------------------------------
%%% @author lycaon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 8 2014 18:16
%%%-------------------------------------------------------------------
-module(mmmario_event_handler).
-author("lycaon").

-behaviour(gen_event).

%% API
-export([
  start_link/0,
  add_handler/1,
  remove_handler/2
]).

%% gen_event callbacks
-export([init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(evtstate, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler(Pid) ->
  HandlerId = {?MODULE, make_ref()},
  gen_event:add_handler(Pid, HandlerId, []),
  HandlerId.

remove_handler(Pid, HandlerId) ->
  gen_event:delete_handler(Pid, HandlerId, []),
  HandlerId.

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
  {ok, #evtstate{}}.

handle_event(update_chara_pos, S) ->
  io:format("event fired!~n"),
  Children = mmmario_player_sup:children(),
  io:format("children: ~p~n", [Children]),
  {ok, S};
handle_event(_Event, S) ->
  {ok, S}.

handle_call(_Request, S) ->
  Reply = ok,
  {ok, Reply, S}.

handle_info(_Info, S) ->
  {ok, S}.

terminate(_Arg, _S) ->
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
