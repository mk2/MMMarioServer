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
  add_handler/0,
  remove_handler/1,
  notify/1
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
%%% 公開APIs
%%%===================================================================

start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler() ->
  HandlerId = {?MODULE, make_ref()},
  gen_event:add_handler(?SERVER, HandlerId, []),
  HandlerId.

remove_handler(HandlerId) ->
  gen_event:delete_handler(?SERVER, HandlerId, []),
  ok.

notify(Event) ->
  gen_event:notify(?SERVER, Event).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
  {ok, #evtstate{}}.

%% キャラの位置更新イベントを受け取る
%% 受け取ったら全キャラの位置情報を取得し、一斉にクライアントに投げる。微妙？
handle_event({update_chara_pos, SenderPPid}, S) ->
  CharaPos = [mmmario_player:get_pos(PPid) || PPid <- mmmario_player_sup:childPids(), PPid =/= SenderPPid],
  case mmmario_event_helper:pos_list_to_binary(CharaPos) of
    {ok, Str} ->
      [mmmario_wsserv:send(WSServPid, Str) || WSServPid <- mmmario_wsserv_sup:childPids()];
    _ -> ok
  end,
  io:format("character positions (except event fire process): ~p~n", [CharaPos]),
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
